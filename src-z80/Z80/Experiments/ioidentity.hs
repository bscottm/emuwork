module Main where

import Control.Applicative (Applicative(..))
import Control.Monad.Identity (Identity(..))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State (StateT, runStateT, execStateT, modify)
import Data.Char

liftIOIntoIdentity :: (a, s) -> IO s -> IO (a, s)
liftIOIntoIdentity (a, s) ms = do
  _ <- liftIO ms
  return (a, runIdentity (Identity s))

-- Define a StateT action
myAction :: StateT Int IO ()
myAction = do
  -- Lift an IO action into StateT
  lift $ putStrLn "Running IO action"
  -- Modify the state
  modify (+1)

-- Run the StateT action and lift the resulting state into Identity
main :: IO ()
main = do
  s <- execStateT myAction 0 >>= execStateT myAction >>= runStateT myAction
  ((), s') <- liftIOIntoIdentity s (putStrLn "Running IO action in main" >> return 2)
  print s'


-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Functor m => Functor (MaybeT m) where
    fmap f = MaybeT . fmap (fmap f) . runMaybeT

instance Applicative m => Applicative (MaybeT m) where
  pure x = MaybeT $ pure (Just x)
  MaybeT mf <*> MaybeT mx = MaybeT $ liftA2 (<*>) mf mx

instance MonadTrans MaybeT where
  lift = MaybeT . fmap Just

instance (Monad m) => Monad (MaybeT m) where
  -- return = lift . return
  x >>= f = MaybeT $ do
    v <- runMaybeT x
    case v of
      Nothing -> return Nothing
      Just y  -> runMaybeT (f y)

readUserName' :: MaybeT IO String
readUserName' = MaybeT $ do
  putStrLn "Please enter your Username!"
  str <- getLine
  if length str > 5
    then return $ Just str
    else return Nothing

readEmail' :: MaybeT IO String
readEmail' = MaybeT $ do
  putStrLn "Please enter your Email!"
  str <- getLine
  if length str > 5
    then return $ Just str
    else return Nothing

readPassword' :: MaybeT IO String
readPassword' = MaybeT $ do
  putStrLn "Please enter your Password!"
  str <- getLine
  if length str < 8 || not (any isUpper str) || not (any isLower str)
    then return Nothing
    else return $ Just str

login :: String -> String -> String -> IO ()
login user email pass =
    putStrLn ("User " ++ user ++ " and email " ++ email ++ " logging in with " ++ pass)

main2 :: IO ()
main2 = do
  maybeCreds <- runMaybeT $ do
    usr <- readUserName'
    email <- readEmail'
    pass <- readPassword'
    return (usr, email, pass)
  case maybeCreds of
    Nothing -> print ("Couldn't login!" :: String)
    Just (u, e, p) -> login u e p