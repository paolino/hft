{-# language StandaloneDeriving,GeneralizedNewtypeDeriving,FlexibleInstances #-}

module Test where 

import Test.QuickCheck
import Data.List
import  HFT


deriving instance Arbitrary (P Int)

pushpop :: [P Int] -> Bool
pushpop =  (==) <$> (unfoldr pop . foldr push E) <*> id

main :: IO ()
main = quickCheck pushpop
