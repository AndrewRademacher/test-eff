{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types                #-}

main :: IO ()
main = undefined

--

newtype Eff a = Eff { runEff :: forall w. (a -> VE w) -> VE w }

instance Functor Eff where
   fmap = undefined

instance Applicative Eff where
   pure     = undefined
   _ <*> _  = undefined

instance Monad Eff where
   return x = Eff $ \k -> k x
   m >>= f  = Eff $ \k -> runEff m (\v -> runEff (f v) k)

data VE w = Val w | E (Int -> VE w)

ask :: Eff Int
ask = Eff (\k -> E k)

admin :: Eff w -> VE w
admin (Eff m) = m Val

runReader :: Eff w -> Int -> w
runReader m e = loop (admin m)
   where
      loop :: VE w -> w
      loop (Val x) = x
      loop (E k)   = loop (k e)
