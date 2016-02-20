{-# LANGUAGE DeriveFunctor #-}

module Main where

data MiniIo a
   = Terminate
   | PrintLine String a
   | ReadLine (String -> a)
   deriving (Functor)

{-instance Functor MiniIo where-}
   {-_ `fmap` Terminate     = Terminate-}
   {-f `fmap` PrintLine s a = PrintLine s (f a)-}
   {-f `fmap` ReadLine g    = ReadLine (f . g) -}

main :: IO ()
main = putStrLn "FunctorExample"
