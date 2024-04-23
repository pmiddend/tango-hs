{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Control.Applicative.Free (Ap (Ap, Pure), liftAp, runAp, runAp_)
import Control.Monad.Free (Free)
import Control.Monad.Identity (Identity (Identity))
import Data.IORef (IORef)
import Data.List (singleton)
import Text.Read (readMaybe)

-- data Access a where
--   ReadValue :: String -> Access String

data Property a = Property
  { propName :: String,
    propReader :: String -> Maybe a
  }
  deriving (Functor)

data MyProperties = MyProperties
  { prop1 :: String,
    prop2 :: Int
  }
  deriving (Show)

prop :: String -> (String -> Maybe a) -> Ap Property a
prop name reader = liftAp $ Property name reader

myProperties :: Ap Property MyProperties
myProperties = MyProperties <$> prop "prop1" Just <*> prop "prop2" readMaybe

propNames :: Ap Property a -> [String]
propNames = runAp_ (singleton . propName)

resolveProperties :: Ap Property a -> IO a
resolveProperties = runAp deconstruct
  where
    deconstruct (Property {propName, propReader}) = do
      propValue <- readProperty propName
      case propReader propValue of
        Nothing -> error "shit"
        Just propValue' -> pure propValue'

newtype Properties = Properties [String]

newtype Instance = Instance (IO ())

libraryInit :: [String] -> IO () -> IO Instance
libraryInit strings cb = do
  putStrLn ("library_init: " <> show strings)
  pure (Instance cb)

libraryStart :: Instance -> IO ()
libraryStart (Instance cb) =
  cb

readProperty :: String -> IO String
readProperty p' = do
  putStrLn ("reading " <> p')
  pure "1"

-- run :: Free Access (IO ()) -> IO ()
-- run accesses = do
--   library_init (getProperties accesses) afterInit
--   library_start
--   where
--     afterInit ptr = join (runAp (\case ReadValue prop -> read_value ptr prop) accesses)

-- getProperties :: Free Access a -> [Property]

main = do
  print $ propNames myProperties
  let startCallback = do
        myResolved <- resolveProperties myProperties
        putStrLn $ "in start callback: " <> show myResolved
  instancePtr <- libraryInit ["prop1", "prop2"] startCallback
  libraryStart instancePtr

-- run accesses
