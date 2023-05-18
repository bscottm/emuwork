{-# LANGUAGE  CPP #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

{-| General-purpose system memory, for both traditional memory and for port-based I/O systems.

Memory is a collection of address regions ('addrType') stored in an 'IntervalMap'. The 'IntervalMap' stores the start
and end addresses for the region; each region may hold a `RAMRegion` (random access memory), a `ROMRegion` (read-only
memory) or a device (`DevRegion`). Both RAM and ROM regions store content as vectors of a `wordType`.
Devices manage content as they see fit, although they may only return or store a `wordType`-sized item.

Non-empty memory regions __/may not/__ overlap; they __/must/__ be distinct.

Writing to a RAM region places the modified word into a priority search queue that acts as a LRU cache.
This amortizes the penalty from updating the underlying content vector until the queue's size is
exceeded (currently 71 elements.)

How to use: Add regions to an `initialMemorySystem` via `mkRAMRegion`, `mkROMRegion` or `mkDevRegion`. Example snippet
from the testing code that creates two RAM regions, 0x0-0x0fff and 0x1400-0x23ff:

> type TestMemSystem = M.MemorySystem Word16 Word8
>
> let msys  = M.mkRAMRegion 0x1400 0x1000 (M.mkRAMRegion 0 0x1000 M.initialMemorySystem :: TestMemSystem)
> in  ...

Alternatively, you could use `mempty` instead of `initialMemorySystem`:

> let msys  = M.mkRAMRegion 0x1400 0x1000 (M.mkRAMRegion 0 0x1000 mempty :: TestMemSystem)
> in  ...

 -}

module Machine.MemorySystem
  ( -- * Exported types
    MemorySystem(..)
  , DeviceRegion(..)
  , MemRead
  , MemReadN
  -- * 'Monoid' interface (see 'MemorySystem' instance for 'Monoid')
  , initialMemorySystem
  , combineMemorySystems
{-
  -- * Memory Regions
  , mkRAMRegion
  , mkROMRegion
  , mkDevRegion
  , msysRAMRegion
  , msysROMRegion
  , msysDevRegion
  -- * Memory read primitives
-}
  , mReadN
{-
  , mRead
  -- * Memory write primitives
  , mWrite
  , mWriteN
  , mPatch
  -- * Utility functions
  , countRegions
  , regionList
  , sanityCheck
-}
  ) where

import           Control.Arrow                    (first, second)
import           Control.Monad                    (when)
import           Control.Monad.Trans.State.Strict (execState, modify, modify', state)

import qualified Data.Foldable                    as Fold
import qualified Data.IntervalMap.Interval        as I
import qualified Data.IntervalMap.Strict          as IM
import           Data.List                        (intercalate)
import           Data.Maybe                       (fromMaybe)
import qualified Data.OrdPSQ                      as OrdPSQ
import           Data.Semigroup                   ()
import           Data.Vector.Unboxed              (Vector, (!))
import qualified Data.Vector.Unboxed              as DVU
import Data.Typeable ( Typeable, cast )
import Data.Kind (Type)
import Data.Word

import           Lens.Micro.Platform              (Lens', Lens, over, to, view, (%~), (&), (-~), (.~), (^.))

import qualified Machine.Device                   as D
import qualified Machine.MachineRAM               as MRAM
import           Machine.Utils                    (ShowHex (as0xHexS))
import Data.Text (Text)


-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- Types
-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

-- | The memory system is a collection of memory region intervals.
newtype MemorySystem addrType wordType = MSys { _regions :: IM.IntervalMap addrType DeviceRegion }

-- | Each of the memory system's regions contain devices.
data DeviceRegion where
  EmptyRegion :: DeviceRegion
  DeviceRegion :: 
      forall addrType wordType devType.
        ( Typeable devType
        , Typeable addrType
        , Typeable wordType
        , Eq devType
        , Show devType
        )
      =>
      { deviceName :: Text
      , theDevice  :: D.Device devType addrType wordType
      } -> DeviceRegion

instance Eq DeviceRegion where
  EmptyRegion == EmptyRegion = True
  DeviceRegion name dev == DeviceRegion name' dev' = name == name' && cast dev == Just dev'
  _ == _ = False

instance Show DeviceRegion where
  show EmptyRegion = "EmptyRegion"
  show (DeviceRegion name dev) = shows ("DevRegion(" :: String)
                                       (shows name (shows (", " :: String)
                                                   (shows dev ")")))

-- | Admit MemorySystem to the Semigroup class (prerequisite for Monoid)
instance ( Integral addrType
         , Integral wordType
         , DVU.Unbox wordType
         , Show addrType
         , ShowHex addrType
         , Show wordType
         ) =>
         Semigroup (MemorySystem addrType wordType) where
  (<>) = combineMemorySystems

-- Admit MemorySystem into the Monoid class
instance
  ( Integral addrType
  , Bounded addrType
  , Integral wordType
  , DVU.Unbox wordType
  , Show addrType
  , ShowHex addrType
  , Show wordType
  )
  => Monoid (MemorySystem addrType wordType) where
  mempty  = initialMemorySystem
  mappend = (<>)

-- | Lens for the memory region map inside a 'MemorySystem'
regions :: Lens' (MemorySystem addrType wordType) (IM.IntervalMap addrType DeviceRegion)
regions f msys@MSys{ _regions = rgn } = (\regions' -> msys { _regions = regions' }) <$> f rgn

-- | Return type for 'mRead'. Devices __can__ (and usually __do__) change state when read, producing an
-- altered 'MemorySystem'
type MemRead addrType wordType (m :: Type -> Type) = m (wordType, MemorySystem addrType wordType)

-- | Return type for 'mReadN'. Devices __can__ and usually __do__ change state when read, producing an altered
-- 'MemorySystem'.
type MemReadN addrType wordType (m :: Type -> Type) = m (Vector wordType, MemorySystem addrType wordType)

-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- MemorySystem code
-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

-- | Create an empty memory system (alternately, use 'mempty')
initialMemorySystem ::
    ( Ord addrType
    , Bounded addrType
    )
  => MemorySystem addrType wordType
initialMemorySystem = MSys { _regions  = IM.insert (IM.ClosedInterval minBound maxBound) EmptyRegion IM.empty }

-- | Combine two memory systems, if their occupied (non-empty) regions don't overlap. If they overlap, an error is raised.
combineMemorySystems :: ( Integral addrType
                        , Integral wordType
                        , DVU.Unbox wordType
                        , Show addrType
                        , ShowHex addrType
                        , Show wordType
                        )
                     => MemorySystem addrType wordType
                     -> MemorySystem addrType wordType
                     -> MemorySystem addrType wordType
combineMemorySystems msysA msysB =
  if   IM.null (IM.intersection occupiedA occupiedB)
  then execState (Fold.forM_ (IM.assocs occupiedB) insRegion) msysA
  else error "MemorySystem:combineMemorySystems: Overlapping regions between memory systems"
  where
    occupiedA = msysA ^. regions & IM.filter (/= EmptyRegion)
    occupiedB = msysB ^. regions & IM.filter (/= EmptyRegion)
    -- We only care about the altered memory system, so this is what execState's return looks like.
    insRegion (ivl, mem) = modify' $ insertMemRegion (I.lowerBound ivl) (I.upperBound ivl) mem

-- | Internal common code that inserts memory regions.
insertMemRegion ::
  ( Integral addrType
  , Integral wordType
  , DVU.Unbox wordType
  , ShowHex addrType
  , Show addrType
  , Show wordType
  )
  => addrType
  -> addrType
  -> DeviceRegion
  -> MemorySystem addrType wordType
  -> MemorySystem addrType wordType
insertMemRegion sa ea newRegion msys
  | all (== EmptyRegion) (IM.elems intersects)
  = over regions splitRegion msys
  | otherwise
  = error (concat [ "insertMemRegion: "
                  , as0xHexS sa
                  , "-"
                  , as0xHexS ea
                  , " overlaps with "
                  ] ++ intercalate ", " (map formatIntersect $ IM.toList intersects)
          )
  where
    intersects = IM.intersecting (msys ^. regions) (I.ClosedInterval sa ea)
    formatIntersect (rgn, ty) = as0xHexS (I.lowerBound rgn) ++ "-" ++ as0xHexS (I.upperBound rgn) ++ "(" ++ show ty ++ ")"
    firstIntersectKey = fst (IM.findMin intersects)
    splitRegion rgns = leftSplit . IM.insert (I.ClosedInterval sa ea) newRegion <$> rightSplit $ IM.delete firstIntersectKey rgns
    leftSplit rgn'
      | I.lowerBound firstIntersectKey < sa
      = IM.insert (I.ClosedInterval (I.lowerBound firstIntersectKey) (sa - 1)) EmptyRegion rgn'
      | otherwise
      = rgn'
    rightSplit rgn'
      | I.upperBound firstIntersectKey > ea
      = IM.insert (I.ClosedInterval (ea + 1) (I.upperBound firstIntersectKey)) EmptyRegion rgn'
      | otherwise
      = rgn'

-- | Fetch data starting from a memory address. Note: If the address does not correspond to a region (i.e., the address does not
-- intersect a region), zero is returned.
mReadN ::
    ( Integral addrType
    , Integral wordType
    , DVU.Unbox wordType
    , Monad m
    )
  => addrType
  -- ^ Address to read
  -> Int
  -- ^ Number of words to read
  -> MemorySystem addrType wordType
  -- ^ Current memory system state
  -> MemReadN addrType wordType m
  -- ^ Value read and updated system state
mReadN addr nRead msys = undefined

testFunc ::
    ( Monad m
    , Integral addrType
    , Ord addrType
    , Integral wordType
    , DVU.Unbox wordType
    )
  => addrType
  -> Int
  -> MemorySystem addrType wordType
  -> m (DVU.Vector wordType, MemorySystem addrType wordType)

testFunc addr nRead msys = do
  -- Ascending interval list [(k, DeviceRegion)] -> m [(Device, Vector wordData)]
  let memRegions = IM.toAscList ((msys ^. regions) `IM.containing` addr)
  readRegions <- sequence
    [ D.runDeviceReadC $ D.deviceRead devOffset devRead dev
      | (iv, DeviceRegion _devName dev) <- memRegions,
        let startAddr = I.lowerBound iv,
        let endAddr   = I.upperBound iv,
        let devOffset = max startAddr addr - startAddr,
        let devRead   = min (nRead - fromIntegral (startAddr - addr)) (fromIntegral (endAddr - startAddr))
    ]
  let readData = DVU.concat (map snd readRegions)
  -- Read device: [m (dev', Vector wordType)]
  pure (readData, msys)
  -- where
    -- accum' d = (accum d DVU.++)
  -- where
  --   getContent accum _iv EmptyRegion = (accum, EmptyRegion)
  --   getContent accum  iv (DeviceRegion devName dev) = do
  --     (dev', devData) <- D.runDeviceReadC $ D.deviceRead devOffset devRead dev
  --     pure (accum' devData, DeviceRegion devName dev')

testFoo :: IO (DVU.Vector Word8, MemorySystem Word16 Word8)
testFoo =  testFunc 15 5 testRAM
  where
    ramDevice = MRAM.mkAddr16ByteRAM 0 0xffff
    testRAM = insertMemRegion (0 :: Word16) (0x00ff :: Word16) (DeviceRegion "PriRAM" ramDevice) (initialMemorySystem :: MemorySystem Word16 Word8)

{- 
  foldMap getContent ((msys ^. regions) `IM.containing` addr)
  where
    getContent subtree
      | IM.null subtree
      = pure (0, msys)
      | otherwise
      = case mregion of
          EmptyRegion -> (0, msys)
          DeviceRegion _name dev -> do
            (dev', devData) <- D.deviceRead 0 1 dev
            pure (devData, dev')

      where
        (iv, mregion)  = IM.findMin subtree
        addrOffset     = addr - I.lowerBound iv
        offs           = fromIntegral addrOffset
        val            = fromMaybe (view contents mregion ! offs)
                                   (lookupPendingWrite offs (mregion ^. writesPending))
        updateDev dev msys' = msys' & regions %~ IM.update (\_ -> Just (DevRegion dev)) iv
-}


{-
data MemorySystem addrType wordType where
  MSys ::
    { _regions    :: MemRegionMap addrType wordType
    -- ^ Region interval map
    } -> MemorySystem addrType wordType

-- | Show a memory system. Can be awfully verbose.
instance ( Show addrType
         , Show wordType
         , DVU.Unbox wordType
         , Integral wordType
         )
         => Show (MemorySystem addrType wordType) where
  show msys = "MemorySystem(" ++ show (msys ^. regions) ++ ")"

-- | A memory region
data MemoryRegion addrType wordType where
  EmptyRegion :: MemoryRegion addrType wordType
  RAMRegion ::
    { _contents       :: ( DVU.Unbox wordType
                         , Integral wordType
                         )
                      => Vector wordType
    -- ^ Memory region's contents (unboxed vector)
    , _writesPending  :: LRUWriteCache wordType
    -- ^ Pending write LRU cache
    , _nWritesPending :: {-# UNPACK #-} !Int
    -- ^ Size of the LRU cache (avoids the $O(n)$ penalty for calling 'OrdPSQ.size')
    } -> MemoryRegion addrType wordType
  ROMRegion ::
    { _contents       :: ( DVU.Unbox wordType
                         , Integral wordType
                         )
                      => Vector wordType
    -- ^ Memory region's contents (unboxed vector)
    } -> MemoryRegion addrType wordType
  DevRegion ::
    { _rgnDevice      :: D.Device addrType wordType
    } -> MemoryRegion addrType wordType

instance ( Show addrType
         , Show wordType
         , Integral wordType
         , DVU.Unbox wordType
         )
         => Show (MemoryRegion addrType wordType) where
  show EmptyRegion                  = "EmptyRegion"
  show (RAMRegion ctnt wpend npend) = "RAMRegion(" ++ show wpend ++ ", " ++ show ctnt ++ " nPend " ++ show npend ++ ")"
  show (ROMRegion ctnt)             = "ROMRegion(" ++ show ctnt ++ ")"
  show (DevRegion _dev)             = "DevRegion()"

instance ( Eq addrType
         , Integral wordType
         , DVU.Unbox wordType
         )
        => Eq (MemoryRegion addrType wordType) where
  EmptyRegion == EmptyRegion = True
  (RAMRegion l_ctnt l_wpend l_npend) == (RAMRegion r_ctnt r_wpend r_npend) =
    l_ctnt == r_ctnt && l_wpend == r_wpend && l_npend == r_npend
  (ROMRegion l_ctnt) == (ROMRegion r_ctnt) = l_ctnt == r_ctnt
  (DevRegion l_dev ) == (DevRegion r_dev ) = l_dev == r_dev
  _ == _ = False


-- | Lens for a memory region's contents
contents :: ( DVU.Unbox wordType
            , Integral wordType
            )
         => Lens' (MemoryRegion addrType wordType) (Vector wordType)
contents f mregion@(RAMRegion ctnt _ _) = (\content' -> mregion { _contents = content' }) <$> f ctnt
contents f mregion@(ROMRegion ctnt)     = (\content' -> mregion { _contents = content' }) <$> f ctnt
contents f mregion                      = mregion <$ f DVU.empty

-- | Lens for the pending write LRU queue.
writesPending :: Lens' (MemoryRegion addrType wordType) (LRUWriteCache wordType)
writesPending f mregion@RAMRegion{}    = (\wp -> mregion { _writesPending = wp }) <$> f (_writesPending mregion)
-- For types other than 'RAMRegion', it's just the original memory region and no pending writes.
writesPending f mregion                = mregion <$ f (initialLRU 0)

-- | Lens for number of elements in the LRU queue
nWritesPending :: Lens' (MemoryRegion addrType wordType) Int
nWritesPending f mregion@RAMRegion{} = (\n -> mregion { _nWritesPending = n }) <$> f (_nWritesPending mregion)
nWritesPending f mregion             = mregion <$ f 0



-- | LRU write cache's "tick" (time/usage tracking) type
type Tick = Int
-- | Write cache's maximum occupancy type
type Size = Int
-- | Least Recently Used (LRU) write cache. Pending writes are queued in the LRU cache to avoid having to recreate or
-- modify a RAM region's vector, which can be expensive when new values are written to RAM.
data LRUWriteCache wordType =
  LRUWriteCache
    { _lrucNextTick :: {-# UNPACK #-} !Tick
    , _lrucPsq      ::                !(OrdPSQ.OrdPSQ Int Tick wordType)
    , _lrucMaxSize  :: {-# UNPACK #-} !Size
    }
    deriving (Eq, Show)

lrucPsq :: Lens' (LRUWriteCache wordType) (OrdPSQ.OrdPSQ Int Tick wordType)
lrucPsq f psq = (\psq' -> psq { _lrucPsq = psq' }) <$> f (_lrucPsq psq)

-- | Create a new RAM memory region. __/Note:/__ memory regions may not overlap; 'error' will signal if this
-- precondition fails.
mkRAMRegion :: ( Integral addrType
               , ShowHex addrType
               , Show addrType
               , Integral wordType
               , DVU.Unbox wordType
               , Show wordType
               )
            => addrType
            -- ^ RAM region start address
            -> Int
            -- ^ Length of the RAM region
            -> MemorySystem addrType wordType
            -> MemorySystem addrType wordType
mkRAMRegion sa len =
  insertMemRegion sa (sa + fromIntegral (len - 1))
                  RAMRegion { _contents       = DVU.replicate len 0
                            , _writesPending  = initialLRU maxPending
                            , _nWritesPending = 0
                            }

-- | Create a new read-only (ROM) memory region. __/Note:/__ memory regions may not overlap; 'error' will signal if this
-- precondition fails.
mkROMRegion :: ( Integral addrType
               , ShowHex addrType
               , Show addrType
               , Integral wordType
               , DVU.Unbox wordType
               , Show wordType
               )
            => addrType
            -- ^ Region's start address
            -> Vector wordType
            -- ^ ROM image to insert as the region's contents
            -> MemorySystem addrType wordType
            -> MemorySystem addrType wordType
mkROMRegion sa romImg =
  insertMemRegion sa (sa + fromIntegral (DVU.length romImg - 1)) ROMRegion { _contents = romImg }

-- | Create a new device memory region. __/Note:/__ memory regions may not overlap; 'error' will signal if this
-- precondition fails.
mkDevRegion :: ( Integral addrType
               , ShowHex addrType
               , Show addrType
               , Integral wordType
               , DVU.Unbox wordType
               , Show wordType
               )
            => addrType
            -- ^ Region's starting address
            -> Int
            -- ^ Region length
            -> D.Device addrType wordType
            -- ^ The device itself
            -> MemorySystem addrType wordType
            -- ^ The original memory system
            -> MemorySystem addrType wordType
            -- ^ The result memory system
mkDevRegion sa len dev = insertMemRegion sa (sa + fromIntegral (len - 1)) (DevRegion dev)

-- | Make a RAM region in an empty 'MemorySystem'. This is intended to be used with 'mappend' to chain together
-- several memory systems.
msysRAMRegion
  :: ( Integral addrType
     , Bounded addrType
     , ShowHex addrType
     , Show addrType
     , Integral wordType
     , DVU.Unbox wordType
     , Show wordType
     )
     => addrType
     -- ^ RAM region start address
     -> Int
     -- ^ Length of the RAM region
     -> MemorySystem addrType wordType
msysRAMRegion addr size = mkRAMRegion addr size mempty

-- | Make a ROM region in an empty 'MemorySystem'. This is intended to be used with 'mappend' to chain together
-- several memory systems.
msysROMRegion
  :: ( Integral addrType
     , Bounded addrType
     , ShowHex addrType
     , Show addrType
     , Integral wordType
     , DVU.Unbox wordType
     , Show wordType
     )
     => addrType
     -- ^ RAM region start address
     -> Vector wordType
     -- ^ ROM image to insert as the region's contents
     -> MemorySystem addrType wordType
msysROMRegion addr vec = mkROMRegion addr vec mempty

msysDevRegion
  :: ( Integral addrType
     , Bounded addrType
     , ShowHex addrType
     , Show addrType
     , Integral wordType
     , DVU.Unbox wordType
     , Show wordType
     )
     => addrType
     -- ^ Region's starting address
     -> Int
     -- ^ Region length
     -> D.Device addrType wordType
     -- ^ The device itself
     -> MemorySystem addrType wordType
     -- ^ The result memory system
msysDevRegion sa len dev = mkDevRegion sa len dev mempty

-- | Fetch a sequence of words from memory. The start and end addresses do not have reside in the same memory region;
-- gaps between regions will be filled with zeroes.
mReadN :: forall addrType wordType.
          ( Integral addrType
          , Integral wordType
          , DVU.Unbox wordType
#if defined(TEST_DEBUG)
          , PrintfArg addrType
          , PrintfArg wordType
          , Show addrType
          , Show wordType
#endif
          )
       => addrType
       -- ^ Starting address
       -> Int
       -- ^ Number of words to read
       -> MemorySystem addrType wordType
       -- ^ The memory system from which to read
       -> MemReadN addrType wordType
       -- ^ Contents read from memory and updated system state
mReadN sAddr nWords msys
  {- Trivial optimizations: -}
  | nWords == 1
  = first DVU.singleton (mRead sAddr msys)
  {- Otherwise, have to do some work. -}
  | otherwise
  = (result DVU.empty, msys & regions %~ updDevRegions updMem)
  where
    memRegions                  = msys ^. regions & (`IM.intersecting` I.ClosedInterval sAddr eAddr)
    (result, updMem)            = IM.mapAccumWithKey collectReads (DVU.empty DVU.++) memRegions
    eAddr                       = sAddr + fromIntegral (nWords - 1)

    -- The type signature isn't strictly necessary, but was useful whilst developing.
    regionValues :: forall addrType'. (Integral addrType') => I.Interval addrType' -> (Int, Int)
    regionValues iv =
      let
        sRegion    = I.lowerBound iv
        eRegion    = I.upperBound iv
        sa         = max sAddr (fromIntegral sRegion)
        ea         = min eAddr (fromIntegral eRegion)
        sOffs      = fromIntegral (sa - fromIntegral sRegion)
        mLen       = fromIntegral (ea - sa + 1)
      in (sOffs, mLen)

    collectReads accum iv EmptyRegion = ((accum (DVU.generate mLen (const 0)) DVU.++), EmptyRegion)
      where
        (_, mLen) = regionValues iv
    
    collectReads accum iv mr@RAMRegion{} =
      ((accum (DVU.update (mr ^. contents & DVU.slice sOffs mLen) pendWrites) DVU.++), mr)
      where
        (sOffs, mLen) = regionValues iv
        pendWrites = DVU.fromList [ (idx, word)
                                  | (addrOffs, _, word) <- mr ^. writesPending . lrucPsq & OrdPSQ.toList
                                  , let idx = addrOffs - sOffs
                                  , idx < mLen
                                  ]

    collectReads accum iv mr@ROMRegion{} = ((accum (mr ^. contents & DVU.slice sOffs mLen) DVU.++), mr)
      where
        (sOffs, mLen) = regionValues iv

    collectReads accum iv DevRegion{ _rgnDevice = devRegion } =
      let (devReads, devState)    = D.devReadSeq (sAddr - I.lowerBound iv) mLen devRegion
          (_sOffs, mLen)          = regionValues iv
      in  ((accum devReads DVU.++), DevRegion devState )

#if 0
    showProgress = trace (printf "sa 0x%04x ea 0x%04x sRegion 0x%04x eRegion 0x%04x  mLen %5d sOffs %5d"
                                  sa ea sRegion eRegion mLen sOffs)
#endif

-- | Update a memory region interval map with only the device regions in the updating map.
updDevRegions :: ( Ord addrType
                 )
              => MemRegionMap addrType wordType
              -> MemRegionMap addrType wordType
              -> MemRegionMap addrType wordType
updDevRegions updRegions {-origRegions-} = execState $ Fold.forM_ (IM.assocs updRegions) modDevRegion
  where
    modDevRegion (k, v)                    = modify $ \m -> onlyDevRegions v k m
    onlyDevRegions dev@DevRegion{}  k mreg = IM.update (const (Just dev)) k mreg
    onlyDevRegions _mr             _k mreg = mreg


-- | Write a word into the memory system: inserts the word into the pending write LRU cache, flushing the cache to make
-- space if necessary, commiting updates to the underlying region's contents. If the memory region is not 'readWrite',
-- then nothing happens.
mWrite :: ( Integral addrType
          , Integral wordType
          , DVU.Unbox wordType
#if defined(TEST_DEBUG)
          , PrintfArg addrType
          , Show addrType
          , PrintfArg wordType
          , Show wordType
#endif
          )
       => addrType
       -- ^ Address to write to
       -> wordType
       -- ^ Value to write
       -> MemorySystem addrType wordType
       -- ^ Current system state
       -> MemorySystem addrType wordType
       -- ^ Updated system state
mWrite addr val = doWrite True addr (DVU.singleton val)


-- | Write a block of memory, respecting the memory system's type
mWriteN :: ( Integral addrType
           , Integral wordType
           , DVU.Unbox wordType
#if defined(TEST_DEBUG)
           , PrintfArg addrType
           , Show addrType
           , PrintfArg wordType
           , Show wordType
#endif
           )
        => addrType
        -- ^ Starting address
        -> Vector wordType
        -- ^ Content to write
        -> MemorySystem addrType wordType
        -- ^ Current system state
        -> MemorySystem addrType wordType
        -- ^ Updated system state
mWriteN = doWrite True


-- | Patch (forcibly overwrite) memory. This does not obey or check the 'readWrite' flag and will truncate the memory patch
-- if it extends beyond a region's upper bound.
mPatch :: ( Integral addrType
          , Integral wordType
          , DVU.Unbox wordType
#if defined(TEST_DEBUG)
          , PrintfArg addrType
          , Show addrType
          , PrintfArg wordType
          , Show wordType
#endif
          )
       => addrType
       -- ^ Starting address where patch is applied
       -> Vector wordType
       -- ^ The patch
       -> MemorySystem addrType wordType
       -- ^ Memory system to which patch will be applied
       -> MemorySystem addrType wordType
       -- ^ Patched memory system
mPatch = doWrite False


-- | Write to memory: 'mPatch' and 'mWriteN' share this code.
doWrite :: ( Integral addrType
           , Integral wordType
           , DVU.Unbox wordType
#if defined(TEST_DEBUG)
           , PrintfArg addrType
           , Show addrType
           , PrintfArg wordType
           , Show wordType
#endif
           )
        => Bool
        -- ^ Preserve read-only regions: If True, prevents writing to 'ROMRegion's
        -> addrType
        -- ^ Starting address where patch is applied
        -> Vector wordType
        -- ^ The patch
        -> MemorySystem addrType wordType
        -- ^ Memory system to which patch will be applied
        -> MemorySystem addrType wordType
        -- ^ Patched memory system
doWrite readOnly startAddr patch msys = msys & regions  %~ updRegionState
  where
    endAddr = startAddr + fromIntegral nWords - 1
    updMem = IM.mapWithKey writeContents (memRegions msys)
    updRegionState = execState (Fold.forM_ (IM.assocs updMem) $ \(k, v) -> modify' (IM.update (const (Just v)) k))
    nWords = DVU.length patch

    memRegions (MSys mregions) = mregions & (`IM.intersecting` I.ClosedInterval startAddr endAddr)

    -- Common values/variables used in multiple places. Easier to package it up as a
    -- tuple and reduce redundancy.
    regionValues iv =
      let
        sRegion = I.lowerBound iv
        sAddr = max startAddr sRegion
        eRegion = I.upperBound iv
        eAddr = min endAddr eRegion
        sOffset = fromIntegral (sAddr - startAddr)
        nWrite = fromIntegral (eAddr - sAddr + 1)
      in (sRegion, sAddr, sOffset, nWrite, eRegion, eAddr)
    
    -- Splice the patch into the memory region's vector, taking care to observe
    -- boundaries
    spliceWriteVec iv ctnt = DVU.concat [ leftSlice, patchSlice, rightSlice ]
      where
        (sRegion, sAddr, sOffset, nWrite, eRegion, _) = regionValues iv
        lOffset = sAddr - sRegion
        patchSlice  = {-tracePatchSlice-} DVU.slice sOffset nWrite patch
        leftSlice = if lOffset > 0 then {-traceLeftSlice-} DVU.slice 0 (fromIntegral lOffset) ctnt else DVU.empty
        rightSlice = if rightLen > 0 then {-traceRightSlice-} DVU.slice rightStart rightLen ctnt else DVU.empty
        rightStart = fromIntegral lOffset + nWrite
        rightLen   = fromIntegral (eRegion - sRegion + 1) - rightStart

    writeContents _ EmptyRegion = EmptyRegion

    -- Don't molest ROM regions if the readOnly flag is true. Otherwise, we can patch the ROM.
    writeContents iv mr@(ROMRegion{}) = if readOnly then mr else mr & contents %~ spliceWriteVec iv

    writeContents iv mr@(DevRegion{}) =
      DevRegion $ D.devWriteSeq (sAddr - sRegion) patch (_rgnDevice mr)
      where
        sAddr = max startAddr sRegion
        sRegion = I.lowerBound iv

    writeContents iv mr@(RAMRegion{})
      -- Queue into the pending write queue if there's space, otherwise flush the pending writes
      -- and overwrite the vector
      | nWrite < maxPending
      = writeQueueSeq mr
      | otherwise
      = mr & writesPending %~ filterPending
           & contents %~ spliceWriteVec iv
      where
        (sRegion, sAddr, sOffset, nWrite, _, _) = regionValues iv
        lOffset = sAddr - sRegion
        patchSlice  = {-tracePatchSlice-} DVU.slice sOffset nWrite patch
        writeQueueSeq =  execState (Fold.forM_ (zip addrWordsList patchAsList) queueUpdate)
        queueUpdate (a, w) = state (queueLRU (fromIntegral a) w)
        -- writeQueueSeq = sequence [state (queueLRU (fromIntegral a ) w) | (a, w) <- zip addrWordsList patchAsList]
        addrWordsList = [fromIntegral offs + lOffset | offs <- [0..nWrite]]
        patchAsList = DVU.toList patchSlice
        filterPending lru = lru & lrucPsq %~ prunePsq [k | let r = fromIntegral lOffset
                                                         , k <- lru ^. lrucPsq & OrdPSQ.keys
                                                         , r >= k && k < r + nWrite]

#if 0
        traceLeftSlice = trace (printf "leftSlice: slice(0, len %d)" lOffset)
        traceRightSlice = trace (printf "rightSlice slice(%d, len %d)" rightStart rightLen)
        tracePatchSlice = trace (printf "patchSlice: sAddr = 0x%04x sOffset = %d nWrite = %d" sAddr sOffset nWrite)
        showProgress = printf "writeContents: sAddr 0x%04x eAddr 0x%04x sRegion 0x%04x eRegion 0x%04x sOffset %5d nWrite %5d"
                              sAddr eAddr sRegion eRegion sOffset nWrite
#endif


-- | Count the number of regions in the memory system; used primarily for testing and debugging
countRegions :: MemorySystem addrType wordType
             -> Int
countRegions msys = msys ^. regions . to IM.size

-- | Generate a list of the regions, without the contents. (Note: Does not permit changing the region's contents, primarily
-- intended as a test interface)
regionList :: ( Integral wordType
              , DVU.Unbox wordType
              )
           => MemorySystem addrType wordType
           -> [(I.Interval addrType, MemoryRegion addrType wordType)]
regionList msys = [ (i, r & contents .~ DVU.empty) | (i, r) <- IM.toList (msys ^. regions)]

-- | Sanity check the memory system's
sanityCheck :: MemorySystem addrType wordType
            -> Bool
sanityCheck msys =
  let verifyRegionSize reg = reg ^. writesPending . to sizeLRU == reg ^. nWritesPending
  in  all verifyRegionSize (msys ^. regions . to IM.elems)


-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~
-- LRU pending write cache implementation, adapted from the psqueues example source. The pending write cache will
-- not exceed a given size.
-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~

-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- Internal constants
-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

-- | Pending write queue maximum depth
maxPending :: Int
maxPending = 71

-- | Number of pending writes to flush when 'maxPending' is reached
maxFlush :: Int
maxFlush = maxPending `div` 3

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~
-- LRU pending write cache implementation, adapted from the psqueues example source. The pending write cache will
-- not exceed maxPending.
-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~

-- | Create an empty write cache
initialLRU :: Int
         -- ^ Maximum size of the pending write cache
         -> LRUWriteCache wordType
         -- ^ The initial write cache
-- eta reduction: implied first argument, maxSize
initialLRU = LRUWriteCache 0 OrdPSQ.empty
{-# INLINE initialLRU #-}

-- | Calculate the size of the write cache
sizeLRU :: LRUWriteCache wordType
        -> Int
sizeLRU = (^. lrucPsq . to OrdPSQ.size)
{-# INLINEABLE sizeLRU #-}

lookupPendingWrite :: Int
                  -- ^ The address offset to probe
                  -> LRUWriteCache wordType
                  -- ^ The pending write cache
                  -> Maybe wordType
                  -- ^ The associated word value, if found, or 'Nothing'
lookupPendingWrite addr = fmap snd . OrdPSQ.lookup addr <$> view lrucPsq
{-# INLINE lookupPendingWrite #-}

increaseTick :: Tick
             -> OrdPSQ.OrdPSQ Int Tick wordType
             -> Size
             -> LRUWriteCache wordType
increaseTick tick psq maxSize
  | tick < maxBound = LRUWriteCache (tick + 1) psq maxSize
  | otherwise       = retick psq OrdPSQ.empty 0
  where
    retick oldPsq newPsq newTick =
      case OrdPSQ.minView oldPsq of
        Nothing                 -> LRUWriteCache newTick newPsq maxSize
        Just (k, _, v, oldPsq') -> retick oldPsq' (OrdPSQ.insert k newTick v newPsq) (newTick + 1)

-- | Queue a write to 'addr' in the pending write cache, flushing the cache if its size exceeds 'maxPending'.
queueLRU :: ( DVU.Unbox wordType
            , Integral wordType
            )
         => Int
         -- ^ Address offset into the memory region being modified
         -> wordType
         -- ^ New contents at the modified address
         -> MemoryRegion addrType wordType
         -- ^ Memory region to modify (_note_: flushing will modify '_contents')
         -> (wordType, MemoryRegion addrType wordType)
         -- ^ Previously written value, if the modified address was in the pending write queue
queueLRU addrOffs word mr =
  let LRUWriteCache nextTick psq maxSize = mr ^. writesPending
      (oldval, psq')                     = OrdPSQ.alter queueAddr addrOffs psq
      queueAddr Nothing                  = (Nothing, Just (nextTick, word))
      queueAddr (Just (_, oldword))      = (Just oldword, Just (nextTick, word))
      updateNWrites (Just _)             = 0
      updateNWrites Nothing              = 1
      doFlush mreg
        | mreg ^. nWritesPending >= maxPending
        = flushPending maxFlush mr'
        | otherwise
        = mreg
      mr'                            = mr & writesPending  .~ increaseTick (nextTick + 1) psq' maxSize
                                          & nWritesPending +~ updateNWrites oldval
      contentVal                     = (mr ^. contents) ! addrOffs
  in  (fromMaybe contentVal oldval, doFlush mr')
queueLRU addrOffs word mr@RAMRegion { _writesPending=LRUWriteCache nextTick psq maxSize
                                    , _contents=ctnt
                                    , _nWritesPending=pendingWrites
                                    } = 
  case OrdPSQ.alter queueAddr addrOffs psq of
    (oldval, psq') ->
      let doFlush mreg
            | pendingWrites >= maxPending
            = flushPending maxFlush mr'
            | otherwise
            = mreg
          mr' = mr { _writesPending = increaseTick (nextTick + 1) psq' maxSize
                   , _nWritesPending = pendingWrites + updateNWrites oldval
                   }
          contentVal = ctnt ! addrOffs
      in  (fromMaybe contentVal oldval, doFlush mr')
  where
    queueAddr Nothing = (Nothing, Just (nextTick, word))
    queueAddr (Just (_, oldword)) = (Just oldword, Just (nextTick, word))
    updateNWrites (Just _) = 0
    updateNWrites Nothing = 1

queueLRU _ _ _ = undefined

-- | Flush some of the pending writes to a memory region's content vector
flushPending :: ( DVU.Unbox wordType
                , Integral wordType
                )
             => Int
             -- ^ Number of pending writes to commit
             -> MemoryRegion addrType wordType
             -- ^ Region where writes will be committed
             -> MemoryRegion addrType wordType
             -- ^ Updated region
flushPending nFlush mr =
  let -- Ascending list: from least recently to most recently used
      ents       = [(k, v) | (k, _, v) <- take nFlush (mr ^. writesPending . lrucPsq . to OrdPSQ.toAscList)]
      -- Bulk update the underlying contents, then prune the write queue (the power of Lenses!)
  in  mr & contents                %~ flip DVU.update (DVU.fromList [(addrOffs, v) | (addrOffs, v) <- ents])
         & writesPending . lrucPsq %~ prunePsq (map fst ents)
         & nWritesPending          -~ length ents

prunePsq :: [Int]
         -- ^ Address offsets to prune
         -> OrdPSQ.OrdPSQ Int Tick wordType
         -- ^ Ordered priority queue to prune
         -> OrdPSQ.OrdPSQ Int Tick wordType
         -- ^ Resulting ordered priority queue
prunePsq addrOffsets psq = Fold.foldr' OrdPSQ.delete psq addrOffsets
{-# INLINE prunePsq #-}
-}
