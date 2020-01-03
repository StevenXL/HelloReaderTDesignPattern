#!/usr/bin/env stack
-- stack --resolver lts-11.6 script

{-

In order to implement the "ReaderT Design Pattern", we want to adhere to the following guidelines:

1) Define a core datatype (often called "Env")

2) Core datatype will contain (a) runtime configuration and (b) any functions
that we may like to be able to mock and (c) mutable reference (IORef, TVar,
etc.) for mutable state.

3) Application code will live in a "ReaderT Env IO" monad. We can define a type
alias or a newtype wrapper. We can use other monadic stacks for pure cord.

4) Our code should be written against mtl-like interface such as MonadReader and
MonadIO. This will allow us to recover some of the purity we "lost" given that
the base monad in our transformer stack is IO.

 -}

main :: IO ()
main = putStrLn "hello"
