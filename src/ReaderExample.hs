{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Main where

import           Control.Eff
import           Control.Eff.Reader.Strict
import           Data.Void

type Global = Free (Union (Reader Double :> (Reader Int :> Data.Void.Void))) Double


main :: IO ()
main = do
   let v = run $ flip runReader (52::Int)
               $ flip runReader (235.2::Double)
               $ (,) <$> divideGlobal <*> subGlobal
               {-$ do d <- divideGlobal-}
                    {-s <- subGlobal-}
                    {-return (d, s)-}
               {-$ divideGlobal-}
               {-$ subGlobal-}
   print v

divideGlobal :: Global
divideGlobal = do
   i :: Int <- ask
   d :: Double <- ask
   return $ d / (fromIntegral i)

subGlobal :: Global
subGlobal = do
   i :: Int <- ask
   d :: Double <- ask
   return $ d - (fromIntegral i)
