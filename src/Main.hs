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
import           Control.Eff.Exception
import           Control.Eff.Reader.Strict
import           Control.Exception
import           Data.Aeson
import           Data.Aeson.Casing
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.ByteString.Lazy.Char8 as CLBS
import           Data.Typeable
import           Data.Void
import qualified Hasql.Connection           as H
import qualified Hasql.Decoders             as HD
import qualified Hasql.Encoders             as HE
import qualified Hasql.Query                as H
import qualified Hasql.Session              as H
import           Prelude                    hiding (log)
import           System.Environment
import           System.IO

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

{-main :: IO ()-}
{-main = do-}
   {-H.acquire (H.settings "localhost" 5432 "andrew" "" "test_eff") >>= \case-}
      {-Left e -> print e-}
      {-Right conn -> do-}
         {-args <- getArgs-}
         {-i <- case args of-}
            {-[fpath] -> CLBS.split '\n' <$> LBS.readFile fpath-}
            {-_       -> CLBS.split '\n' <$> LBS.hGetContents stdin-}
         {-mapM_ (go conn) i-}
         {-H.release conn-}

{-go :: H.Connection -> LBS.ByteString -> IO ()-}
{-go _ bytes = do-}
   {-case eitherDecode bytes of-}
      {-Left e -> print e-}
      {-Right v -> print (v::Value)-}

-- 24 Days of Hackage Example

{-data Log v = Log String v-}
   {-deriving (Functor, Typeable)-}

{-log :: Member Log r => String -> Eff r ()-}
{-log txt = send $ \next -> inj (Log txt (next ()))-}

{-runLogger :: Eff (Log :> r) a -> Eff r (a, [String])-}
{-runLogger logAction = go (admin logAction)-}
   {-where-}
      {-go (Val v) = return (v, [])-}
      {-go (E request) =-}
         {-let prefixLogWith txt (v, l) = (v, txt:l)-}
             {-performLog (Log txt next) =-}
               {-fmap (prefixLogWith txt) (go next)-}
          {-in handleRelay request go performLog-}

{-verboseAddition :: Member Log r => Eff r Int-}
{-verboseAddition = do-}
   {-log "Start with 1..."-}
   {-x <- return 1-}

   {-log "then add 2..."-}
   {-y <- return 2-}

   {-let r = x + y-}

   {-log $ "The result is " ++ show r-}
   {-return r-}
