{-# LANGUAGE DataKinds #-}

module Units
  ( Angle (..),
    toDegrees,
    toRadians,
    Distance (..),
    toMeters,
    toFeet,
    toYards,
    toMiles,
    toNauticalMiles,
    Mass (..),
    toOunces,
    toPounds,
    toGrams,
    Energy (..),
    toKiloCalories,
    toKiloJoules,
  )
where

import qualified Data.Dimensions.SI as D
import Data.Metrology
import Data.Metrology.SI hiding (Energy, Length, Mass)
import Data.Units.US

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

-- Distance ----------------------------------------------------------------------

data Distance a
  = Meters a
  | Feet a
  | Yards a
  | Miles a
  | NauticalMiles a
  deriving
    ( Show
    )

toDistanceUnit :: (Fractional a) => Distance a -> Qu '[F D.Length One] DefaultLCSU a
toDistanceUnit (Meters m) = m % Meter
toDistanceUnit (Feet f) = f % Foot
toDistanceUnit (Yards y) = y % Yard
toDistanceUnit (Miles m) = m % Mile
toDistanceUnit (NauticalMiles n) = n % NauticalMile

toMeters :: (Fractional a) => Distance a -> Distance a
toMeters (Meters m) = Meters m
toMeters d = Meters $ toDistanceUnit d # Meter

toFeet :: (Fractional a) => Distance a -> Distance a
toFeet (Feet f) = Feet f
toFeet d = Feet $ toDistanceUnit d # Foot

toYards :: (Fractional a) => Distance a -> Distance a
toYards (Yards y) = Yards y
toYards d = Yards $ toDistanceUnit d # Yard

toMiles :: (Fractional a) => Distance a -> Distance a
toMiles (Miles m) = Miles m
toMiles d = Miles $ toDistanceUnit d # Mile

toNauticalMiles :: (Fractional a) => Distance a -> Distance a
toNauticalMiles (NauticalMiles n) = NauticalMiles n
toNauticalMiles d = NauticalMiles $ toDistanceUnit d # NauticalMile

-- Mass ------------------------------------------------------------------------

data Mass a
  = Ounces a
  | Pounds a
  | Grams a
  deriving (Show)

toMassUnits :: (Fractional a) => Mass a -> Qu '[F D.Mass One] DefaultLCSU a
toMassUnits (Pounds p) = p % Pound
toMassUnits (Ounces o) = o % Ounce
toMassUnits (Grams g) = g % Gram

toOunces :: (Fractional a) => Mass a -> Mass a
toOunces (Ounces o) = Ounces o
toOunces m = Ounces $ toMassUnits m # Ounce

toPounds :: (Fractional a) => Mass a -> Mass a
toPounds (Pounds p) = Pounds p
toPounds m = Pounds $ toMassUnits m # Pound

toGrams :: (Fractional a) => Mass a -> Mass a
toGrams (Grams g) = Grams g
toGrams m = Grams $ toMassUnits m # Gram

-- Energy ----------------------------------------------------------------------

data Energy a
  = KiloCalories a
  | KiloJoules a
  deriving (Show)

toEnergyUnits ::
  (Fractional a) =>
  Energy a ->
  Qu
    [F D.Mass One, F D.Length (S (S Zero)), F D.Time (P (P Zero))]
    DefaultLCSU
    a
toEnergyUnits (KiloCalories kc) = kc % FoodCalorie
toEnergyUnits (KiloJoules kj) = kj % (Kilo :@ Joule)

toKiloCalories :: (Fractional a) => Energy a -> Energy a
toKiloCalories (KiloCalories kc) = KiloCalories kc
toKiloCalories e = KiloCalories $ toEnergyUnits e # FoodCalorie

toKiloJoules :: (Fractional a) => Energy a -> Energy a
toKiloJoules (KiloJoules kj) = KiloJoules kj
toKiloJoules e = KiloJoules $ toEnergyUnits e # Kilo :@ Joule
