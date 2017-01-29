module Bench where
import Criterion.Main
import Test.QuickCheck

import Test

ns = [1000,10000,100000,1000000]

benchSuite = do

    xs <- mapM (\n -> generate (vectorOf n arbitrary)) ns
    defaultMain [
        bgroup "pushpop" $ 
            zipWith (\n x -> bench (show n)  $ whnf pushpop x) ns xs
        ]
