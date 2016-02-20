module Main where

data It i a
   = Pure a
   | Get (i -> It i a)

instance Functor (It i) where
   fmap = undefined

instance Applicative (It i) where
   pure    = undefined
   _ <*> _ = undefined

instance Monad (It i) where
   return = Pure
   
   Pure x >>= k = k x
   Get k' >>= k = Get (k'>>> k)

(>>>) :: (Monad m) => (a -> m b) -> (b -> m c) -> (a -> m c)
f >>> g = (>>= g) . f

main :: IO ()
main = putStrLn "Reader Impl Example"

ask :: It i i
ask = Get Pure

addGet :: Int -> It Int Int
addGet x = ask >>= \i -> return (i + x)

addN :: Int -> It Int Int
addN n = foldl (>>>) return (replicate n addGet) 0

runReader :: i -> It i a -> a
runReader _ (Pure v) = v
runReader x (Get  k) = runReader x (k x) 
