{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Machine.Device 
  ( DeviceIO(..)
  , DeviceReadC(..)
  , deviceRead
  , DeviceWriteC(..)
  , deviceWrite
  )
where

import           Control.Algebra        (Algebra (..), Has, send, type (:+:) (..))
import           Control.Monad.IO.Class (MonadIO (..))

import           Data.Kind              (Type)
import           Data.Typeable          (Typeable, cast)
import qualified Data.Vector.Unboxed    as DVU

--------------------------------------------------------------------------------
-- "Tubular Bells" approach to managing simulator devices via modular algebraic
-- effects.
--------------------------------------------------------------------------------

-- "Grand Piano..."

-- | The DeviceIO type class: The minimal interface to support a device, which includes RAM and ROM.
-- This introduces a functional dependency where the device type, 'devType', uniquely determines
-- the device's internal address type, 'addrType', and its basic units, 'wordType'.
--
-- .. Devices read starting at an offset specified by 'addrType' and a number of basic units
--    (usually bytes, aka 'Word8'), returning the (potentially mutated) device state and a 'Vector'
--    of basic units read.
-- .. Devices write a 'Vector' of basic units (again, usually bytes, aka 'Word8') at an offset
--    specified by 'addrType', returning the (potentially mutated) device state.
--
-- Type class instances can make reading and writing a lot more efficient. For example, RAM and ROM
-- devices can optimize reading and writing when the number of bytes to read equals 1 (direct indexing
-- into a 'Vector' vice 'Vector' slicing.) 
class
    ( Integral addrType
    , Integral wordType
    , DVU.Unbox wordType
    )
  => DeviceIO devType addrType wordType where

  -- | The data family of devices
  data family Device devType addrType wordType

  doDevRead ::
      ( MonadIO m
      )
    => addrType
    -> Int
    -> Device devType addrType wordType
    -> m (Device devType addrType wordType, DVU.Vector wordType)

  doDevWrite ::
      ( MonadIO m
      )
    => addrType
    -> DVU.Vector wordType
    -> Device devType addrType wordType
    -> m (Device devType addrType wordType)

instance {-# OVERLAPPABLE #-}
  Show devType
  => Show (Device devType addrType wordType) where
    show dev = shows ("Device(generic: " :: String) (shows dev ")")

instance {-# OVERLAPPABLE #-}
    ( Eq devType
    , Typeable devType
    , Typeable addrType
    , Typeable wordType
    )
  => Eq (Device devType addrType wordType) where
    dev == dev' = cast dev == Just dev'

-- "Reed and pipe organ"

-- | The device read effect (type). Note that this matches the the 'doDeviceRead' function signature in the first
-- three arguments.
data DeviceRead addrType wordType devType (m :: Type -> Type) (k :: Type) where
    DeviceRead ::
        ( DeviceIO devType addrType wordType
        ) =>
      addrType ->
      Int ->
      Device devType addrType wordType ->
      DeviceRead addrType wordType (Device devType addrType wordType)  m (Device devType addrType wordType, DVU.Vector wordType)

-- | The general device reader function that packages the 'DeviceRead' effect and invokes the action.
deviceRead ::
    ( Has (DeviceRead addrType wordType (Device devType addrType wordType)) sig m
    , DeviceIO devType addrType wordType
    ) =>
  addrType ->
  Int ->
  Device devType addrType wordType ->
  m (Device devType addrType wordType, DVU.Vector wordType)
deviceRead addr nRead dev = send (DeviceRead addr nRead dev)

-- "Glockenspiel"

-- | The device reader's carrier. Note: Invoke 'runDeviceReadC` to extract the returned (inner) monad
-- from the action, e.g., when inside of an 'IO' monad 'do' block.
newtype DeviceReadC addrType wordType devType m a = DeviceReadC { runDeviceReadC :: m a }
    deriving (Functor, Applicative, Monad, MonadIO)

-- "Bass Guitar"

instance
    -- So long as the 'm' monad can interpret the 'sig' effects (and also
    -- perform IO), conforms to the DeviceIO type class' interface...
    ( Algebra sig m
    , MonadIO m
    , DeviceIO devType addrType wordType
    , Integral addrType
    , Integral wordType
    , DVU.Unbox wordType
    )
    -- ... the 'DeviceRead addrType wordType devType m' monad can interpret
    -- 'DeviceReadC addrType wordType devType m :+: sig' effects
  => Algebra (DeviceRead addrType wordType (Device devType addrType wordType) :+: sig) (DeviceReadC addrType wordType devType m) where

  alg hdl sig ctx = case sig of
    (L (DeviceRead addr nRead dev)) -> (<$ ctx) <$> doDevRead addr nRead dev
    R other                         -> DeviceReadC (alg (runDeviceReadC . hdl) other ctx)

-- "Double-speed guitar"

-- | The device write effect.
data DeviceWrite addrType wordType (dev :: Type) (m ::  Type -> Type) (k :: Type) where
  DeviceWrite ::
      ( DeviceIO devType addrType wordType
      ) =>
    addrType ->
    DVU.Vector wordType ->
    Device devType addrType wordType ->
    DeviceWrite addrType wordType (Device devType addrType wordType) m (Device devType addrType wordType)

deviceWrite ::
  ( Has (DeviceWrite addrType wordType (Device devType addrType wordType)) sig m
  , DeviceIO devType addrType wordType
  ) =>
  addrType ->
  DVU.Vector wordType ->
  Device devType addrType wordType ->
  m (Device devType addrType wordType)
deviceWrite addr devData dev = send (DeviceWrite addr devData dev)

-- "Two slightly distorted guitars!"

-- Device read carrier
newtype DeviceWriteC addrType wordType devType m a = DeviceWriteC { runDeviceWriteC :: m a }
    deriving (Functor, Applicative, Monad, MonadIO)

-- "Mandolin!"

instance
    -- So long as the 'm' monad can interpret the 'sig' effects (and also
    -- perform IO), conforms to the DeviceIO type class' interface...
    ( Algebra sig m
    , MonadIO m
    , DeviceIO devType addrType wordType
    , DVU.Unbox wordType
    )
    -- ... the 'DeviceWrite addrType wordType devType m' monad can interpret
    -- 'DeviceWriteC addrType wordType devType m :+: sig' effects
  => Algebra (DeviceWrite  addrType wordType (Device devType addrType wordType) :+: sig)
             (DeviceWriteC addrType wordType devType m) where

  alg hdl sig ctx = case sig of
    (L (DeviceWrite addr devData dev)) -> (<$ ctx) <$> doDevWrite addr devData dev
    R other                            -> DeviceWriteC (alg (runDeviceWriteC . hdl) other ctx)
