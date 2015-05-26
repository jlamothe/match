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

module Tests.DecodeListing (tests) where

import Match.Pure
import Match.Types
import Test.HUnit (Test (..), (@=?))

tests :: Test
tests = TestLabel "Match.Pure.decodeListing" $
  TestList [ emptyStringTest
           , badInputTest
           , unexpectedJSONTest
           , goodDataTest
           ]

emptyStringTest :: Test
emptyStringTest = TestLabel "empty string" $
  TestCase $ Nothing @=? decodeListing ""

badInputTest :: Test
badInputTest = TestLabel "bad input" $
  TestCase $ Nothing @=? decodeListing "foo"

unexpectedJSONTest :: Test
unexpectedJSONTest = TestLabel "unexpected JSON" $
  TestCase $ Nothing @=? decodeListing "[]"

goodDataTest :: Test
goodDataTest = TestLabel "good data" $
  TestCase $ expected @=? decodeListing input

input :: String
input =
  "{\"title\":\"LED Flash Macro Ring Light (48 X LED) with 6 Adapter Rings for For Canon/Sony/Nikon/Sigma Lenses\",\"manufacturer\":\"Neewer Electronics Accessories\",\"currency\":\"CAD\",\"price\":\"35.99\"}"

expected :: Maybe Listing
expected = Just
  Listing { listingTitle        = "LED Flash Macro Ring Light (48 X LED) with 6 Adapter Rings for For Canon/Sony/Nikon/Sigma Lenses"
          , listingManufacturer = "Neewer Electronics Accessories"
          , listingCurrency     = "CAD"
          , listingPrice        = "35.99"
          }

-- jl
