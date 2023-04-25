{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}

module Machine.Device where

import           Control.Monad       (ap, liftM)
import           Control.Monad.Trans (MonadIO (..), MonadTrans (..))

import qualified Data.Vector.Unboxed as DVU

newtype DeviceT addrType wordType fm devType = DeviceT { theDevice :: fm devType }
    -- ^ Device state
    {-
    , devReset  :: DeviceReset addrType wordType m devTag
    -- ^ Device reset: 
    , devRead   :: DeviceReader addrType wordType m devTag
    -- ^ Device reader function. The address is an offset into the device's memory region, which eliminates the device's
    -- need to keep track of its base address.
    , devWrite  :: DeviceWriter addrType wordType m devTag
    -- ^ Device writer function. Writes the word into the specified zero-based offset address. See 'devReader'.
    -- NOTE: The type signature for the function is exactly what 'execState' requires. Yes, it is awkward, but avoids
    -- additional overhead and wrapper functions just to make the signature pretty.
    -}
    -- } -> DeviceT addrType wordType fm devType
  deriving (Eq, Show)

type DeviceReset  addrType wordType fm devTag = fm devTag -> fm devTag
type DeviceReader addrType wordType fm devTag = addrType -> devTag -> (wordType, devTag)
-- type DeviceWriter addrType wordType devTag = DVU.Vector wordType -> addrType -> devTag -> devTag

instance (Monad fm) => Functor (DeviceT addrType wordType fm) where
  fmap = liftM
  {-# INLINE fmap #-}

instance (Monad fm) => Applicative (DeviceT addrType wordType fm) where
  pure = DeviceT . pure
  {-# INLINE pure #-}
  (<*>) = ap
  {-# INLINE (<*>) #-}

instance (Monad fm) => Monad (DeviceT addrType wordType fm) where
  return = pure
  m >>= dev = DeviceT $ theDevice m >>= theDevice . dev
  {-# INLINE (>>=) #-}

instance MonadTrans (DeviceT addrType wordType) where
  lift dev = DeviceT $ do dev
  {-# INLINE lift #-}

instance (MonadIO fm) => MonadIO (DeviceT addrType wordType fm) where
  liftIO = lift . liftIO
  {-# INLINE liftIO #-}

class DeviceIO addrType wordType fm devTag where
  deviceWrite
    :: DVU.Vector wordType
    -> addrType
    -> DeviceT addrType wordType fm devTag
    -> DeviceT addrType wordType fm devTag
