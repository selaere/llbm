module Mode exposing (..)
import Bitwise exposing (or, and, shiftLeftBy)
import Array exposing (..)
import Dict exposing (..)

mode_chars : List Char
mode_chars = String.toList "bwuipscmhaedtnvklgjyrf"

modes_dict : Dict Char Mode
modes_dict = mode_chars |> List.indexedMap (\i->\x-> (x, 2^i)) |> Dict.fromList

modes : List Mode
modes = mode_chars |> List.filterMap fromChar

type alias Mode = Int

fromChar : Char -> Maybe Mode
fromChar char = Dict.get char modes_dict

fromString : String -> Mode
fromString = String.toList >> List.filterMap fromChar >> List.foldl merge 0

toString : Mode -> String
toString mode = mode_chars
    |> List.indexedMap Tuple.pair
    |> List.filterMap (\(i,c) -> if and (shiftLeftBy i 1) mode /= 0 then Just c else Nothing)
    |> String.fromList

merge : Mode -> Mode -> Mode
merge = or

intersect : Mode -> Mode -> Mode
intersect = and