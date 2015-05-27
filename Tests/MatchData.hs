-- Match
-- Copyright (C) 2015  Jonathan Lamothe
-- <jonathan@jlamothe.net>

-- This program is free software: you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation, either version 3 of the
-- License, or (at your option) any later version.

-- This program is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see
-- <http://www.gnu.org/licenses/>.

module Tests.MatchData (tests) where

import Data.Char
import Data.Map ((!))
import qualified Data.Map as Map
import Match.Types
import Match.Pure
import Test.HUnit ( Test (..)
                  , assertBool
                  , (@=?)
                  )

tests :: Test
tests = TestLabel "Match.Pure.matchData" $
  TestList [listMatchTests "keys" (Map.keys expected) (Map.keys actual)]

listMatchTests :: (Eq a, Show a) => String -> [a] -> [a] -> Test
listMatchTests label expected actual =
  TestLabel label $
  TestList [ listCountTest expected actual
           , listComparisonTest expected actual
           ]

listCountTest :: [a] -> [a] -> Test
listCountTest expected actual =
  TestLabel "element counts" $
  TestCase $ length expected @=? length actual

listComparisonTest :: (Eq a, Show a) => [a] -> [a] -> Test
listComparisonTest expected actual =
  TestLabel "element comparison" $
  TestList $ map (listElementTest actual) expected

listElementTest :: (Eq a, Show a) => [a] -> a -> Test
listElementTest list element =
  TestLabel (show element) $
  TestCase $ assertBool (show element ++ " not found in list: " ++ show list) $
  element `elem` list

inputProducts :: [Product]
inputProducts = [productA, productB]

inputListings :: [Listing]
inputListings = [ listingA1
                , listingA2
                , listingB1
                , listingB2
                , listingC
                ]

expected :: MatchedData
expected =
  Map.fromList [ (titleA, [listingA1, listingA2])
               , (titleB, [listingB1, listingB2])
               ]

actual :: MatchedData
actual = matchData inputProducts inputListings

productA :: Product
productA =
  Product { productName         = titleA
          , productManufacturer = manufacturerA
          }

productB :: Product
productB =
  Product { productName         = titleB
          , productManufacturer = manufacturerB
          }

listingA1 :: Listing
listingA1 =
  Listing { listingTitle        = "foo with baz bar"
          , listingManufacturer = map toLower manufacturerA
          , listingCurrency     = "CAD"
          , listingPrice        = "1.00"
          }

listingA2 :: Listing
listingA2 =
  Listing { listingTitle        = "foo bar and baz"
          , listingManufacturer = map toLower manufacturerA
          , listingCurrency     = "CAD"
          , listingPrice        = "2.25"
          }

listingB1 :: Listing
listingB1 =
  Listing { listingTitle        = "foo bar quux"
          , listingManufacturer = map toLower manufacturerB
          , listingCurrency     = "CAD"
          , listingPrice        = "3.00"
          }

listingB2 :: Listing
listingB2 =
  Listing { listingTitle        = "quux and foo bar"
          , listingManufacturer = map toLower manufacturerB
          , listingCurrency     = "CAD"
          , listingPrice        = "5.50"
          }

listingC :: Listing
listingC =
  Listing { listingTitle        = "fooby bar baz"
          , listingManufacturer = map toLower manufacturerA
          , listingCurrency     = "CAD"
          , listingPrice        = "6.15"
          }

titleA :: String
titleA = "Foo_Bar_Baz"

titleB :: String
titleB = "Foo_Quux_Bar"

manufacturerA :: String
manufacturerA = "XYZ Inc."

manufacturerB :: String
manufacturerB = "Some Guy"

-- jl
