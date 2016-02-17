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
import           Control.Eff.Lift
import           Control.Eff.Reader.Strict
import           Data.Void
import           System.Random

type Global a = Free (Union (Reader Double :> (Reader Int :> (Lift IO :> Void)))) a

main :: IO ()
main = do
   v <- runLift
      $ flip runReader (52::Int)
      $ flip runReader (235.2::Double)
      $ (,,) <$> divideGlobal <*> subGlobal <*> mulRand
   print v

divideGlobal
   :: ( Member (Reader Int) e
      , Member (Reader Double) e
      )
   => Eff e Double
divideGlobal = do
   i :: Int <- ask
   d :: Double <- ask
   return $ d / (fromIntegral i)

subGlobal
   :: ( Member (Reader Int) e
      , Member (Reader Double) e
      )
   => Eff e Double
subGlobal = do
   i :: Int <- ask
   d :: Double <- ask
   return $ d - (fromIntegral i)

mulRand
   :: ( Member (Reader Double) e
      , SetMember Lift (Lift IO) e
      )
   => Eff e Double
mulRand = do
   x :: Double <- ask
   y :: Double <- lift randomIO
   return $ x * y
