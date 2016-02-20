{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}

module Main where

import           Control.Eff
import           Control.Eff.Lift
import           Data.Functor.Contravariant
import           Data.Int
import           Data.Text                  (Text)
import           Data.Typeable
import qualified Hasql.Connection           as H
import qualified Hasql.Decoders             as HD
import qualified Hasql.Encoders             as HE
import qualified Hasql.Pool                 as HP
import qualified Hasql.Query                as H
import qualified Hasql.Session              as H

main :: IO ()
main = do
   pool <- HP.acquire (4, 60.0, hset)
   r <- runLift $ runHasql pool $
      db $ appendMessage "Hello World!"
   print r
   where
      hset = H.settings "localhost" 5432 "andrew" "" "test_eff"

data Hasql a
   = Hasql (H.Session a)
   deriving (Functor, Typeable)

runHasql
   :: SetMember Lift (Lift IO) r
   => HP.Pool -> Eff (Hasql :> r) a -> Eff r (Either HP.UsageError a)
runHasql pool = loop
   where
      loop = freeMap
               (return . Right)
               (\u -> handleRelay u loop act)
      act (Hasql s) = lift $ HP.use pool s >>= \case
         Left  e -> return $ Left e
         Right v -> return $ Right v

db :: (Member Hasql e) => H.Session a -> Eff e a
db = send . inj . Hasql

appendMessage :: Text -> H.Session Int64
appendMessage txt = sess
   where
      sess    = H.query txt $ H.statement sql encoder decoder True
      sql     = "INSERT INTO messages (content) VALUES ($1) RETURNING message_id"
      encoder = contramap id (HE.value HE.text)
      decoder = HD.singleRow $ HD.value HD.int8

getMessage :: Int64 -> H.Session Text
getMessage mid = sess
   where
      sess    = H.query mid $ H.statement sql encoder decoder True
      sql     = "SELECT content FROM messages WHERE message_id = $1"
      encoder = contramap id (HE.value HE.int8)
      decoder = HD.singleRow $ HD.value HD.text
