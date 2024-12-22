import Test.Tasty
import Test.Tasty.HUnit

-- Dummy -----------------------------------------------------------------------

dummy :: TestTree
dummy = testCase "dummy" $ assertEqual "two plus two equals four" (2 + 2) (4 :: Integer)

-- Main ------------------------------------------------------------------------

main :: IO ()
main = defaultMain dummy
