{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Foldable
import qualified Data.Map.Strict as M
import Units

-- Nutrition Facts -------------------------------------------------------------

data NutFacts = NutFacts
  { nfServingSize :: Mass Double,
    nfCalories :: Energy Double,
    nfTotalFat :: Mass Double,
    nfCholesterol :: Mass Double,
    nfSodium :: Mass Double,
    nfTotalCarb :: Mass Double,
    nfTotalSugars :: Mass Double,
    nfProtein :: Mass Double,
    nfNutrients :: M.Map Nutrient NutrientQuantity
  }
  deriving (Show)

instance Semigroup NutFacts where
  (<>) = combineNutFacts

instance Monoid NutFacts where
  mempty =
    NutFacts
      { nfServingSize = defaultUnits 0,
        nfCalories = defaultUnits 0,
        nfTotalFat = defaultUnits 0,
        nfCholesterol = defaultUnits 0,
        nfSodium = defaultUnits 0,
        nfTotalCarb = defaultUnits 0,
        nfTotalSugars = defaultUnits 0,
        nfProtein = defaultUnits 0,
        nfNutrients = mempty
      }

data Nutrient
  = VitaminA
  | VitaminC
  | VitaminB1
  | VitaminB2
  | VitaminB3
  | VitaminB5
  | VitaminB6
  | VitaminB9
  | VitaminB7
  | VitaminB12
  | VitaminD
  | VitaminE
  | VitaminK
  | Calcium
  | Iron
  | Phosphorus
  | Iodine
  | Magnesium
  | Zinc
  | Selenium
  | Copper
  | Manganese
  | Chromium
  | Molybdenum
  | Chloride
  | Potassium
  deriving (Eq, Ord, Show, Bounded, Enum)

data NutrientQuantity
  = NutrientPctDV Int
  | NutrientMass (Mass Double)
  deriving (Show)

dailyValue :: Nutrient -> (Mass Double)
dailyValue VitaminA = MicroGrams 900
dailyValue VitaminB1 = MilliGrams 1.5
dailyValue VitaminB2 = MilliGrams 1.7
dailyValue VitaminB3 = MilliGrams 20
dailyValue VitaminB5 = MilliGrams 10
dailyValue VitaminB6 = MilliGrams 2
dailyValue VitaminB9 = MicroGrams 400
dailyValue VitaminB7 = MicroGrams 300
dailyValue VitaminB12 = MicroGrams 6
dailyValue VitaminC = MilliGrams 60
dailyValue VitaminD = MicroGrams 20
dailyValue VitaminE = MilliGrams 12
dailyValue VitaminK = MicroGrams 80
dailyValue Calcium = Grams 1
dailyValue Iron = MilliGrams 18
dailyValue Phosphorus = Grams 1
dailyValue Iodine = MicroGrams 150
dailyValue Magnesium = MilliGrams 400
dailyValue Zinc = MilliGrams 15
dailyValue Selenium = MicroGrams 70
dailyValue Copper = MilliGrams 2
dailyValue Manganese = MilliGrams 2
dailyValue Chromium = MicroGrams 120
dailyValue Molybdenum = MicroGrams 45
dailyValue Chloride = MicroGrams 75
dailyValue Potassium = Grams 4.7

nqToMass :: Nutrient -> NutrientQuantity -> Mass Double
nqToMass _ (NutrientMass m) = m
nqToMass n (NutrientPctDV p) = mulUnits (fromIntegral p / 100) (dailyValue n)

-- NutFacts Utils ---------------------------------------------------------------

combineNutFacts :: NutFacts -> NutFacts -> NutFacts
combineNutFacts nf1 nf2 =
  NutFacts
    { nfServingSize = nfServingSize nf1 `addUnits` nfServingSize nf2,
      nfCalories = nfCalories nf1 `addUnits` nfCalories nf2,
      nfTotalFat = nfTotalFat nf1 `addUnits` nfTotalFat nf2,
      nfCholesterol = nfCholesterol nf1 `addUnits` nfCholesterol nf2,
      nfSodium = nfSodium nf1 `addUnits` nfSodium nf2,
      nfTotalCarb = nfTotalCarb nf1 `addUnits` nfTotalCarb nf2,
      nfTotalSugars = nfTotalSugars nf1 `addUnits` nfTotalSugars nf2,
      nfProtein = nfProtein nf1 `addUnits` nfProtein nf2,
      nfNutrients =
        M.unionWithKey
          (\k n1 n2 -> NutrientMass $ addUnits (nqToMass k n1) (nqToMass k n2))
          (nfNutrients nf1)
          (nfNutrients nf2)
    }

effectiveNutFacts :: NutFacts -> Mass Double -> NutFacts
effectiveNutFacts NutFacts {..} m =
  let scale = unUnits m / unUnits nfServingSize
      su :: (Units u) => u Double -> u Double
      su = mulUnits scale
   in NutFacts
        { nfServingSize = su nfServingSize,
          nfCalories = su nfCalories,
          nfTotalFat = su nfTotalFat,
          nfCholesterol = su nfCholesterol,
          nfSodium = su nfSodium,
          nfTotalCarb = su nfTotalCarb,
          nfTotalSugars = su nfTotalSugars,
          nfProtein = su nfProtein,
          nfNutrients =
            M.mapWithKey
              (\n nq -> NutrientMass $ su $ nqToMass n nq)
              nfNutrients
        }

-- Sample Foods ----------------------------------------------------------------

threePepperColbyJack :: NutFacts
threePepperColbyJack =
  NutFacts
    { nfServingSize = Ounces 1,
      nfCalories = KiloCalories 100,
      nfTotalFat = Grams 8,
      nfCholesterol = MilliGrams 25,
      nfSodium = MilliGrams 170,
      nfTotalCarb = Grams 1,
      nfTotalSugars = Grams 0,
      nfProtein = Grams 6,
      nfNutrients =
        M.fromList
          [ (VitaminD, NutrientPctDV 0),
            (Calcium, NutrientPctDV 15),
            (Iron, NutrientPctDV 0),
            (Potassium, NutrientPctDV 0)
          ]
    }

colbyJack :: NutFacts
colbyJack =
  NutFacts
    { nfServingSize = Ounces 1,
      nfCalories = KiloCalories 110,
      nfTotalFat = Grams 9,
      nfCholesterol = MilliGrams 25,
      nfSodium = MilliGrams 180,
      nfTotalCarb = Grams 0,
      nfTotalSugars = Grams 0,
      nfProtein = Grams 6,
      nfNutrients =
        M.fromList
          [ (VitaminD, NutrientPctDV 0),
            (Calcium, NutrientPctDV 15),
            (Iron, NutrientPctDV 0),
            (Potassium, NutrientPctDV 0)
          ]
    }

firesmithChicken :: NutFacts
firesmithChicken =
  NutFacts
    { nfServingSize = Ounces 2,
      nfCalories = KiloCalories 60,
      nfTotalFat = Grams 1.5,
      nfCholesterol = MilliGrams 35,
      nfSodium = MilliGrams 340,
      nfTotalCarb = Grams 1,
      nfTotalSugars = Grams 0,
      nfProtein = Grams 11,
      nfNutrients =
        M.fromList
          [ (VitaminA, NutrientPctDV 0),
            (VitaminC, NutrientPctDV 0),
            (Calcium, NutrientPctDV 0),
            (Iron, NutrientPctDV 2)
          ]
    }

everroastChicken :: NutFacts
everroastChicken =
  NutFacts
    { nfServingSize = Ounces 2,
      nfCalories = KiloCalories 60,
      nfTotalFat = Grams 1,
      nfCholesterol = MilliGrams 30,
      nfSodium = MilliGrams 440,
      nfTotalCarb = Grams 0,
      nfTotalSugars = Grams 1,
      nfProtein = Grams 10,
      nfNutrients =
        M.fromList
          [ (VitaminA, NutrientPctDV 0),
            (VitaminC, NutrientPctDV 2),
            (Calcium, NutrientPctDV 0),
            (Iron, NutrientPctDV 2)
          ]
    }

evtSour :: NutFacts
evtSour =
  NutFacts
    { nfServingSize = Grams 32,
      nfCalories = KiloCalories 70,
      nfTotalFat = Grams 0,
      nfCholesterol = MilliGrams 0,
      nfSodium = MilliGrams 170,
      nfTotalCarb = Grams 14,
      nfTotalSugars = Grams 0,
      nfProtein = Grams 2,
      nfNutrients =
        M.fromList
          [ (Iron, NutrientPctDV 0),
            (Calcium, NutrientMass $ MilliGrams 21),
            (Potassium, NutrientMass $ MilliGrams 32),
            (VitaminD, NutrientPctDV 0)
          ]
    }

pb :: NutFacts
pb =
  NutFacts
    { nfServingSize = Grams 32,
      nfCalories = KiloCalories 200,
      nfTotalFat = Grams 17,
      nfCholesterol = MilliGrams 0,
      nfSodium = MilliGrams 0,
      nfTotalCarb = Grams 7,
      nfTotalSugars = Grams 1,
      nfProtein = Grams 8,
      nfNutrients =
        M.fromList
          [ (Iron, NutrientPctDV 4),
            (Calcium, NutrientPctDV 2),
            (VitaminA, NutrientPctDV 0),
            (VitaminC, NutrientPctDV 0)
          ]
    }

-- Recipe ----------------------------------------------------------------------

newtype Recipe = Recipe [(Mass Double, NutFacts)] deriving (Show)

netRecipe :: Recipe -> NutFacts
netRecipe (Recipe igs) = foldMap (\(m, nf) -> effectiveNutFacts nf m) igs

-- Main ------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "\n---------------------------------------------------------------\n"
  print $ fold [firesmithChicken, colbyJack, evtSour]
  putStrLn "\n---------------------------------------------------------------\n"
  print $
    netRecipe
      ( Recipe
          [ (Grams 150, firesmithChicken),
            (Grams 70, colbyJack),
            (Grams 70, evtSour)
          ]
      )
