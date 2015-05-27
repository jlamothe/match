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

module Tests.Decodable.Common (tests) where

import Match.Decodable
import Test.HUnit (Test (..), (@=?))

tests :: (Decodable d, Eq d, Show d) => String -> d -> String -> Test
tests label expected input =
  TestLabel label $
  TestList $ map test
  [ ("empty input",     Nothing,       ""   )
  , ("invalid input",   Nothing,       "foo")
  , ("unexpected JSON", Nothing,       "[]" )
  , ("valid input",     Just expected, input)
  ]

test :: (Decodable d, Eq d, Show d) => (String, Maybe d, String) -> Test
test (label, expected, input) =
  TestLabel label $
  TestCase $ expected @=? decode input

-- jl
