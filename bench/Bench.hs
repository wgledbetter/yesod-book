import Criterion.Main

-- Dummy -----------------------------------------------------------------------

dummy :: Benchmark
dummy = bench "addition" $ whnf (+2) (2 :: Integer)

-- Main ------------------------------------------------------------------------

main :: IO ()
main = defaultMain [dummy]
