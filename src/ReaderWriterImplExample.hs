module Main where

data It i o a
   = Pure a
   | Get   ( i -> It i o a)
   | Put o (() -> It i o a)

instance Functor (It i o) where
   fmap = undefined

instance Applicative (It i o) where
   pure    = undefined
   _ <*> _ = undefined

instance Monad (It i o) where
   return = Pure
   
   Pure  x  >>= k =        k x
   Get   k' >>= k = Get   (k'>>> k)
   Put x k' >>= k = Put x (k' >>> k) 

(>>>) :: (Monad m) => (a -> m b) -> (b -> m c) -> (a -> m c)
f >>> g = (>>= g) . f

main :: IO ()
main = putStrLn "Reader/Writer Impl Example"

ask :: It i o i
ask = Get Pure

addGet :: (Num a) => a -> It a o a
addGet x = ask >>= \i -> return (i + x)

addN :: (Num a) => Int -> It a o a
addN n = foldl (>>>) return (replicate n addGet) 0

--

tell :: o -> It i o ()
tell o = Put o Pure

runRdWriter :: (Monoid o) => i -> It i o a -> (a, o)
runRdWriter i = loop mempty
   where
      loop acc (Pure x)  = (x, acc)
      loop acc (Get k)   = loop acc (k i)
      loop acc (Put o k) = loop (o `mappend` acc) (k ())
