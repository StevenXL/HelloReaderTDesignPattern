#!/usr/bin/env stack
-- stack --resolver lts-14.19 script

{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import Test.Hspec (hspec, describe, it, shouldBe)

import qualified Control.Monad.Writer.Lazy as Writer
import qualified Control.Monad.State.Lazy as State

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

class Monad m => OutPut m where
    outPut :: String -> m ()

instance OutPut (ReaderT Env IO) where
    outPut :: String -> ReaderT Env IO ()
    outPut msg = liftIO $ putStrLn msg

instance OutPut (ReaderT String (Writer.Writer String)) where
    outPut :: String -> ReaderT String (Writer.Writer String) ()
    outPut msg = Writer.tell msg

instance OutPut (ReaderT String (State.State String)) where
    outPut :: String -> ReaderT String (State.State String) ()
    outPut msg = State.put msg

main :: IO ()
main = hspec $ do -- 1) Using a different monadic stack, we can test IO / effects in a pure way
    describe "program" $ do
        it "outputs the correct greeting, Writer" $ do
            let res = Writer.execWriter $ runReaderT program "Ada Lovelace"
            res `shouldBe` "Hello, Ada Lovelace"
        it "outputs the correct greeting, State" $ do
            let res = State.execState (runReaderT program "Ada Lovelace") ""
            res `shouldBe` "Hello, Ada Lovelace"


initializeEnv :: IO Env
initializeEnv = do
    name <- prompt "Enter a name" >> getLine
    return (Env { envName = name })

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
