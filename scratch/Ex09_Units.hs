-- I did something like this at Sandia and was pretty pleased with myself.

module Main where

import Data.Metrology
import Data.Metrology.SI

-- Angles ----------------------------------------------------------------------

data Angle a
  = Degrees a
  | Radians a
  deriving (Show)

toDegrees :: (Fractional a) => Angle a -> Angle a
toDegrees (Degrees d) = Degrees d
toDegrees (Radians r) = Degrees $ (r % Radian) # Degree

toRadians :: (Fractional a) => Angle a -> Angle a
toRadians (Radians r) = Radians r
toRadians (Degrees d) = Radians $ (d % Degree) # Radian

-- Main ------------------------------------------------------------------------

main :: IO ()
main = do
  print $ toDegrees (Radians (pi :: Double))
  print $ toRadians (Degrees (45 :: Double))
