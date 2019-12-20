module Utils.String exposing 
  ( interpolate
  , trimInnerWhitespace
  )

-- Imports ---------------------------------------------------------------------
import Regex
import Set exposing (Set)
import Tuple.Extra exposing (Tuple)

-- Functions -------------------------------------------------------------------
{- A pale imitation of javascript's template strings. The set of tuples correspond
to the string to replace, and the string to replace it with. The tags to
replace must be wrapped in ${ } in the source string. 
-}
interpolate : String -> Set (Tuple String String) -> String
interpolate template replacements =
  Set.foldl  (\(t, r) s -> String.replace ("${" ++ t ++ "}") r s) template replacements

{- When parsing the record field types, I grab everything up until a separator
like '}' or ','. This is necessary to capture types like `Maybe (List String)`
that have spaces in them. This regex is just a way to trim any inner whitespace
in the string in case someone types in a record with weird formatting.
-}
trimInnerWhitespace : String -> String
trimInnerWhitespace string =
  Regex.fromStringWith { caseInsensitive = False, multiline = True } "\\s{2,}"
    |> Maybe.map (\r -> Regex.replace r (always " ") string)
    |> Maybe.withDefault string
