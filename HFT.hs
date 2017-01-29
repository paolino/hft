{-# language MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances#-}

-- Finger tree half spine, amortized O(1) access to head (push/pop) , O(log n) access to elements (peek)

module HFT (F(E),push,pop,Peek(peek),P(..)) where

import Control.Applicative
import Data.List
import Data.Maybe

data T a = S !Int a | B !Int a a  deriving Show

class Peek s b c where
    peek :: Int -> s b -> Maybe c

instance Peek T c c where
    peek 0 (S _ x) = Just x
    peek 0 (B _ x _) = Just x 
    peek 1 (B _ x y) = Just y
    peek _ _ = Nothing

instance Peek T a c => Peek T (T a) c where
    peek n (S k x)  = peek n x
    peek n (B k x y) 
        | n < px = peek n x
        | otherwise = peek (n - px) y                
        where px = power x

class Power a where
    power :: a -> Int

instance Power (T a) where
    power (S k _) = k
    power (B k _ _) = k

newtype P a = P {unP :: a} deriving (Eq,Show)

instance Power (P a) where
    power _ = 1

data F a  = F (T a) (F (T a)) | E deriving Show

push :: Power a => a -> F a -> F a 
push x = let 
    px = power x 
    f E = F (S px x) E
    f (F (S py y) e) = F (B (px + py) x y) e
    f (F b e) = F (S px x) $ push b e
    in f

pop :: Power a => F a -> Maybe (a, F a)
pop E = Nothing
pop (F (B _ x y) e) = Just (x, F (S (power y) y) e)
pop (F (S _ x) e) = Just (x, maybe E (uncurry F) $ pop e)

instance Peek T a b => Peek F a b where
    peek n E = Nothing
    peek n (F x e) = peek n x <|> peek (n - power x) e 
   
-- naive push 10 * 10^6 elements to an F and random access them all
main = do
    let s = map P is
        is =  [1..10000000::Int]
    let t = foldl' (flip push) E $ s 
    print . sum . map (\s -> unP $ fromJust (peek s t :: Maybe (P Int))) $ is
