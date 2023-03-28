{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}

module Machine.Device
  ( Device(..)
  , DeviceReset
  , DeviceReader
  , DeviceWriter
  , devReadSeq
  , devWriteSeq
  ) where

import           Prelude                           hiding (words)
import           Control.Arrow                     ((***))
import           Control.Monad.Trans.State.Strict  (state, runState, execState)
import qualified Data.Vector.Unboxed               as DVU

#if defined(TEST_DEBUG)
import           Debug.Trace
import           Text.Printf
#endif

data Device addrType wordType where
  Device ::
    { theDevice :: devTag
    -- ^ Device state
    , devReset  :: DeviceReset addrType wordType devTag
    , devRead   :: DeviceReader addrType wordType devTag
    -- ^ Device reader function. The address is an offset into the device's memory region, which eliminates the device's
    -- need to keep track of its base address.
    , devWrite  :: DeviceWriter addrType wordType devTag
    -- ^ Device writer function. Writes the word into the specified zero-based offset address. See 'devReader'.
    -- NOTE: The type signature for the function is exactly what 'execState' requires. Yes, it is awkward, but avoids
    -- additional overhead and wrapper functions just to make the signature pretty.
    } -> Device addrType wordType

instance Eq (Device addrType wordType) where
  devA == devB = devA == devB

type DeviceReset  addrType wordType devTag = devTag -> devTag
type DeviceReader addrType wordType devTag = addrType -> devTag -> (wordType, devTag)
type DeviceWriter addrType wordType devTag = wordType -> addrType -> devTag -> ((), devTag)

-- | The workhorse for reading a data sequence from a device, returning both the resulting list
devReadSeq :: ( Integral addrType
              , Integral wordType
              , DVU.Unbox wordType
#if defined(TEST_DEBUG)
              , Show addrType
#endif
              )
           => addrType
           -> Int
           -> Device addrType wordType
           -> (DVU.Vector wordType, Device addrType wordType)
devReadSeq sOffs mLen (Device dev' reset reader writer) =
  DVU.fromList *** (\dev -> Device dev reset reader writer) $ runState devReadState dev'
  where
      devReadState = sequence [state (reader a) | a <- [sOffs..sOffs + fromIntegral mLen - 1]]


-- | The workhorse for writing a data sequence from a device, returning updated memory region
devWriteSeq :: ( Integral addrType
               , Integral wordType
               , DVU.Unbox wordType
#if defined(TEST_DEBUG)
               , PrintfArg addrType
               , PrintfArg wordType
#endif
               )
            => addrType
            -> DVU.Vector wordType
            -> Device addrType wordType
            -> Device addrType wordType
devWriteSeq sOffs words (Device dev' reset reader writer) =
  Device (execState devWriteState dev') reset reader writer
  where
    wordLen        = DVU.length words
    wordsAsList    = DVU.toList words
    addrsAsList    = [sOffs..sOffs + fromIntegral wordLen - 1]
    devWriteState  = sequence [state (writeTrace w a $ writer w a) | (w, a) <- zip wordsAsList addrsAsList]
#if defined(TEST_DEBUG)
    writeTrace w a = trace (printf "devWriteSeq: %08x = %04x" a w)
#else
    writeTrace _w _a = id
#endif
