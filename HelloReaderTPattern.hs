#!/usr/bin/env stack
-- stack --resolver lts-14.19 script

{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import Test.Hspec (hspec, describe, it, shouldBe)

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

instance HasName String where
    getName :: String -> String
    getName str = str

class Monad m => OutPut m where -- 1) Extract the effect of outputting content to a typeclass
    outPut :: String -> m ()

instance OutPut (ReaderT Env IO) where
    outPut :: String -> ReaderT Env IO ()
    outPut msg = liftIO $ putStrLn msg

main :: IO ()
main = do
    env <- initializeEnv
    runReaderT program env

initializeEnv :: IO Env
initializeEnv = do
    name <- prompt "Enter a name" >> getLine
    return (Env { envName = name })

-- 2) By removing the MonadIO constraint (and replacing it with OutPut
-- constraint), computations defined in terms of `program` can no longer perform
-- arbitrary IO.
program :: (HasName env, OutPut m, MonadReader env m) => m ()
program = do
    greeting <- computeGreeting
    outPut greeting

computeGreeting :: (HasName env, MonadReader env m) => m String
computeGreeting = do
    name <- getName <$> ask
    pure $ ("Hello, " <> name)

prompt :: String -> IO ()
prompt str = putStr (str <> ": ")
