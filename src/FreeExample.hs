module Main where

--

data Free f a
   = Pure a
   | Impure (f (Free f a))

instance Functor (Free f) where
   fmap _ = undefined

instance Applicative (Free f) where
   pure    = undefined
   _ <*> _ = undefined

instance (Functor f) => Monad (Free f) where
   return = Pure

   Pure   a >>= k = k a
   Impure f >>= k = Impure (fmap (>>= k) f)

--

data ReaderWriter i o x
   = Get (i -> x)
   | Put o (() -> x)

instance Functor (ReaderWriter i o) where
   fmap f (Get k) = Get (f . k)
   fmap f (Put o k) = Put o (f . k)

type It i o a = Free (ReaderWriter i o) a

main :: IO ()
main = putStrLn "Free Example"
