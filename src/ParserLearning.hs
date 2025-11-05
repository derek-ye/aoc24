module ParserLearning () where

import Prelude

newtype MyIdentity a = MyIdentity a

instance Functor MyIdentity where
    fmap f (MyIdentity a) = MyIdentity $ f a

instance Applicative MyIdentity where
    pure = MyIdentity
    (MyIdentity f) <*> (MyIdentity x) = MyIdentity $ f x

instance Monad MyIdentity where
    (>>=) (MyIdentity a) f = f a

newtype MyStringReader a = MyStringReader (String -> a) -- where a = (x -> y)

instance Functor MyStringReader where
    fmap f (MyStringReader g) = MyStringReader $ f . g

instance Applicative MyStringReader where
    pure :: a -> MyStringReader a
    pure a = MyStringReader $ const a
    (MyStringReader f) <*> (MyStringReader g) = MyStringReader $ \x -> (f x) $ (g x)

instance Monad MyStringReader where
    (>>=) :: MyStringReader a -> (a -> MyStringReader b) -> MyStringReader b
    (>>=) (MyStringReader (f :: String -> a)) (g :: a -> MyStringReader b) = MyStringReader $ \x ->
        let (MyStringReader h) = (g $ f x)
            in h x

parseInt :: MyStringReader Int
parseInt = MyStringReader $ read

newtype MyStringState a = MyStringState (String -> (a, String))

instance Functor MyStringState where
    fmap :: (a -> b) -> MyStringState a -> MyStringState b
    fmap f (MyStringState (g :: String -> (a, String))) = MyStringState $ \x -> 
        let (a, s) = g x
            in (f a, s)

instance Applicative MyStringState where
    pure :: a -> MyStringState a
    pure a = MyStringState $ \s -> (a, s)
    -- <*> :: MyStringState (a -> b) -> MyStringState a -> MyStringState b
    MyStringState (f :: String -> (a -> b, String)) <*> MyStringState (g :: String -> (a, String)) = MyStringState $ \s ->
        let (f', s') = f s
            (a, s'') = g s'
            in (f' a, s'')

-- instance Monad MyStringState where


-- parseInt :: MyStringState Int
-- parseInt = MyStringState $ read
