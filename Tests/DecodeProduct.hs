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

module Tests.DecodeProduct (tests) where

import Match.Pure
import Match.Types
import Test.HUnit (Test (..), (@=?))

tests :: Test
tests = TestLabel "decodeProducts" $
  TestList $ [ emptyStringTest
             , badInputTest
             , unexpectedJSONTest
             , goodDataTest
             ]

emptyStringTest :: Test
emptyStringTest = TestLabel "empty string" $
  TestCase $ Nothing @=? decodeProduct ""

badInputTest :: Test
badInputTest = TestLabel "bad input" $
  TestCase $ Nothing @=? decodeProduct "foo"

unexpectedJSONTest :: Test
unexpectedJSONTest = TestLabel "unexpected JSON" $
  TestCase $ Nothing @=? decodeProduct "[]"

goodDataTest :: Test
goodDataTest = TestLabel "good data" $
  TestCase $ expected @=? decodeProduct input

input :: String
input =
  "{\"product_name\":\"Sony_Cyber-shot_DSC-W310\",\"manufacturer\":\"Sony\",\"model\":\"DSC-W310\",\"family\":\"Cyber-shot\",\"announced-date\":\"2010-01-06T19:00:00.000-05:00\"}"

expected :: Maybe Product
expected = Just $
  Product { productName = "Sony_Cyber-shot_DSC-W310"
          , productManufacturer = "Sony"
          }

-- jl
