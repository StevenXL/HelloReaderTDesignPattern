#!/usr/bin/env stack
-- stack --resolver lts-14.19 script

{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import Test.Hspec (hspec, describe, it, shouldBe) -- 1) Import testing framework

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

main :: IO () -- 2) Write a spec for the computeGreeting computation
main = hspec $ do
    describe "computeGreeting" $ do
        it "computes the correct greeting" $ do
            let ada = "Ada Lovelace" -- 3) Thanks to the "Has- typeclass approach", we can execute computeGreeting with only the environment it cares about
            res <- runReaderT computeGreeting ada
            res `shouldBe` "Hello, Ada Lovelace"

initializeEnv :: IO Env
initializeEnv = do
    name <- prompt "Enter a name" >> getLine
    return (Env { envName = name })

program :: (HasName env, MonadIO m, MonadReader env m) => m ()
program = do
    greeting <- computeGreeting
    liftIO $ putStrLn greeting

computeGreeting :: (HasName env, MonadReader env m) => m String
computeGreeting = do
    name <- getName <$> ask
    pure $ ("Hello, " <> name)

prompt :: String -> IO ()
prompt str = putStr (str <> ": ")
