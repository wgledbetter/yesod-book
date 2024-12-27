{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Units
  ( Units,
    unUnits,
    defaultUnits,
    addUnits,
    mulUnits,
    Angle (..),
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
    toMilliGrams,
    toMicroGrams,
    Energy (..),
    toKiloCalories,
    toKiloJoules,

    -- * Testing
    unitIdentity,
  )
where

import qualified Data.Dimensions.SI as D
import Data.Metrology
import Data.Metrology.SI hiding (Energy, Length, Mass)
import Data.Units.US

-- Units Typeclass -------------------------------------------------------------

class (Functor u) => Units u where
  {-# MINIMAL unUnits, defaultUnits #-}

  unUnits :: (Fractional a) => u a -> a
  defaultUnits :: a -> u a

  addUnits :: (Fractional a) => u a -> u a -> u a
  addUnits u1 u2 = defaultUnits $ x1 + x2
    where
      x1 = unUnits u1
      x2 = unUnits u2

  mulUnits :: (Num a) => a -> u a -> u a
  mulUnits c = fmap (* c)

-- Property which must hold for all units:
unitIdentity :: forall u a. (Units u, Fractional a, Eq a) => u a -> Bool
unitIdentity u = u' == u''
  where
    u' = unUnits u
    d :: u a
    d = defaultUnits u'
    u'' = unUnits d

-- Angles ----------------------------------------------------------------------

data Angle a
  = Degrees a
  | Radians a
  deriving (Show, Functor)

instance Units Angle where
  defaultUnits = Radians
  unUnits = toRadians

toDegrees :: (Fractional a) => Angle a -> a
toDegrees (Degrees d) = d
toDegrees (Radians r) = (r % Radian) # Degree

toRadians :: (Fractional a) => Angle a -> a
toRadians (Radians r) = r
toRadians (Degrees d) = (d % Degree) # Radian

-- Distance ----------------------------------------------------------------------

data Distance a
  = Meters a
  | Feet a
  | Yards a
  | Miles a
  | NauticalMiles a
  deriving (Show, Functor)

instance Units Distance where
  defaultUnits = Meters
  unUnits = toMeters

toDistanceUnit :: (Fractional a) => Distance a -> Qu '[F D.Length One] DefaultLCSU a
toDistanceUnit (Meters m) = m % Meter
toDistanceUnit (Feet f) = f % Foot
toDistanceUnit (Yards y) = y % Yard
toDistanceUnit (Miles m) = m % Mile
toDistanceUnit (NauticalMiles n) = n % NauticalMile

toMeters :: (Fractional a) => Distance a -> a
toMeters (Meters m) = m
toMeters d = toDistanceUnit d # Meter

toFeet :: (Fractional a) => Distance a -> a
toFeet (Feet f) = f
toFeet d = toDistanceUnit d # Foot

toYards :: (Fractional a) => Distance a -> a
toYards (Yards y) = y
toYards d = toDistanceUnit d # Yard

toMiles :: (Fractional a) => Distance a -> a
toMiles (Miles m) = m
toMiles d = toDistanceUnit d # Mile

toNauticalMiles :: (Fractional a) => Distance a -> a
toNauticalMiles (NauticalMiles n) = n
toNauticalMiles d = toDistanceUnit d # NauticalMile

-- Mass ------------------------------------------------------------------------

data Mass a
  = Ounces a
  | Pounds a
  | Grams a
  | MilliGrams a
  | MicroGrams a
  deriving (Show, Functor)

instance Units Mass where
  defaultUnits = Grams
  unUnits = toGrams

toMassUnits :: (Fractional a) => Mass a -> Qu '[F D.Mass One] DefaultLCSU a
toMassUnits (Pounds p) = p % Pound
toMassUnits (Ounces o) = o % Ounce
toMassUnits (Grams g) = g % Gram
toMassUnits (MilliGrams m) = m % Milli :@ Gram
toMassUnits (MicroGrams m) = m % Micro :@ Gram

toOunces :: (Fractional a) => Mass a -> a
toOunces (Ounces o) = o
toOunces m = toMassUnits m # Ounce

toPounds :: (Fractional a) => Mass a -> a
toPounds (Pounds p) = p
toPounds m = toMassUnits m # Pound

toGrams :: (Fractional a) => Mass a -> a
toGrams (Grams g) = g
toGrams m = toMassUnits m # Gram

toMilliGrams :: (Fractional a) => Mass a -> a
toMilliGrams (MilliGrams m) = m
toMilliGrams m = toMassUnits m # (Milli :@ Gram)

toMicroGrams :: (Fractional a) => Mass a -> a
toMicroGrams (MicroGrams m) = m
toMicroGrams m = toMassUnits m # (Micro :@ Gram)

-- Energy ----------------------------------------------------------------------

data Energy a
  = KiloCalories a
  | KiloJoules a
  deriving (Show, Functor)

instance Units Energy where
  defaultUnits = KiloCalories
  unUnits = toKiloCalories

toEnergyUnits ::
  (Fractional a) =>
  Energy a ->
  Qu
    [F D.Mass One, F D.Length (S (S Zero)), F D.Time (P (P Zero))]
    DefaultLCSU
    a
toEnergyUnits (KiloCalories kc) = kc % FoodCalorie
toEnergyUnits (KiloJoules kj) = kj % (Kilo :@ Joule)

toKiloCalories :: (Fractional a) => Energy a -> a
toKiloCalories (KiloCalories kc) = kc
toKiloCalories e = toEnergyUnits e # FoodCalorie

toKiloJoules :: (Fractional a) => Energy a -> a
toKiloJoules (KiloJoules kj) = kj
toKiloJoules e = toEnergyUnits e # Kilo :@ Joule

-- Tests -----------------------------------------------------------------------
