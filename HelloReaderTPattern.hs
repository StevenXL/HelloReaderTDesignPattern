#!/usr/bin/env stack
-- stack --resolver lts-11.6 script

{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.Reader (ReaderT, MonadReader)
import Control.Monad.Reader (runReaderT, reader)

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Class (liftIO)

-- 1) Core datatype
data Env = Env { envName :: String }

main :: IO ()
main = do
    env <- initializeEnv -- 2) Initialize runtime configuration in main
    runReaderT program env

initializeEnv :: IO Env
initializeEnv = do
    name <- prompt "Enter a name" >> getLine
    return (Env { envName = name })

program :: (MonadIO m, MonadReader Env m) => m () -- 3) Write functions in terms of mtl-style typeclasses
program = do
  name <- reader envName
  liftIO $ putStrLn ("Hello" <> " " <> name)

prompt :: String -> IO ()
prompt str = putStr (str <> ": ")
