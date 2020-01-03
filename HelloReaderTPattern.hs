#!/usr/bin/env stack
-- stack --resolver lts-11.6 script

{-# LANGUAGE InstanceSigs #-}

import Control.Monad.Reader (ReaderT, MonadReader)
import Control.Monad.Reader (runReaderT, ask)

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Class (liftIO)

data Env = Env { envName :: String }

class HasName env where
    getName :: env -> String

instance HasName Env where
    getName :: Env -> String
    getName env = envName env

main :: IO ()
main = do
    env <- initializeEnv
    runReaderT program env

initializeEnv :: IO Env
initializeEnv = do
    name <- prompt "Enter a name" >> getLine
    return (Env { envName = name })

-- Our program has no knowledge of the Env data type. All it requires is that
-- the environment has an instance of HasName.
program :: (HasName env, MonadIO m, MonadReader env m) => m ()
program = do
    name <- getName <$> ask
    liftIO $ putStrLn ("Hello" <> " " <> name)

prompt :: String -> IO ()
prompt str = putStr (str <> ": ")
