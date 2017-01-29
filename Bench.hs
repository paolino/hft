module Bench where
import Criterion.Main
import Test.QuickCheck

import Test

benchSuite = do
    x1 <- generate (vectorOf 1000 arbitrary)
    x10 <- generate (vectorOf 10000 arbitrary)
    x100 <- generate (vectorOf 100000 arbitrary)
    defaultMain [
        bgroup "pushpop" [ bench "1000"  $ whnf pushpop x1
               , bench "10000"  $ whnf pushpop x10
               , bench "100000"  $ whnf pushpop x100
               ]
        ]
