{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}

module ConsoleExample where

import           Control.Eff
import           Control.Eff.Lift
import           Data.Text        (Text)
import qualified Data.Text.IO     as T
import           Data.Typeable

main :: IO ()
main = do
   r <- runLift $ runConsoleLogging $ do
      logM ("Hello"::String)
      logM ("World"::String)
      return (1 + 2)
   print (r::Int)

data Logger n
   = Logger String (() -> n)
   deriving (Typeable)

deriving instance Functor Logger

logM :: (Member Logger r) => String -> Eff r ()
logM msg = send . inj $ Logger msg id

runConsoleLogging
   :: SetMember Lift (Lift IO) r
   => Eff (Logger :> r) a -> Eff r a
runConsoleLogging = freeMap
  return
  (\u -> handleRelay u runConsoleLogging act)
   where
     act (Logger msg k) = do
       lift $ putStrLn msg
       runConsoleLogging (k ())

{-main :: IO ()-}
{-main = do-}
   {-r <- runLift $ runConsoleGetting $ do-}
      {-l1 <- getM-}
      {-l2 <- getM-}
      {-return (l1, l2)-}
   {-print (r::(Text, Text))-}

{-data Getter n-}
   {-= Getter (Text -> n)-}
   {-deriving (Typeable)-}

{-deriving instance Functor Getter-}

{-getM :: (Member Getter r) => Eff r Text-}
{-getM = send . inj $ Getter id-}

{-runConsoleGetting-}
   {-:: SetMember Lift (Lift IO) r-}
   {-=> Eff (Getter :> r) a -> Eff r a-}
{-runConsoleGetting = loop-}
   {-where-}
      {-loop = freeMap-}
               {-return-}
               {-(\u -> handleRelay u loop act)-}
      {-act (Getter k) = do-}
         {-v <- lift T.getLine-}
         {-loop (k v)-}
