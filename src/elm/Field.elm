module Field exposing
  ( Field
  , fromString, toString
  , Options
  )

-- Imports ---------------------------------------------------------------------
import Set
import String.Extra as String
import Regex exposing (Regex)

-- Types -----------------------------------------------------------------------
type Field
  = Field String String

type alias Options =
  { withTypes : Bool
  , withGenerics : Bool
  , withGetters : Bool
  , withSetters : Bool
  }

-- Constants -------------------------------------------------------------------
--
fields : Regex
fields =
  "(?<=[{,] *)[a-z]\\w*(?= *:)"
    |> Regex.fromString
    |> Maybe.withDefault Regex.never

types : Regex
types =
  "(?<=: *)[A-z]\\w*"
    |> Regex.fromString
    |> Maybe.withDefault Regex.never

-- Functions -------------------------------------------------------------------
--
match : Regex -> String -> List String
match regex string =
  Regex.find regex string
    |> List.map .match

--
fromString : String -> List Field
fromString record =
  let
    fs = match fields record |> Set.fromList |> Set.toList
    ts = match types record
  in
  List.map2 Field fs ts

--
toString : Options -> Field -> String
toString options ( Field f t ) =
  let
    typedUpdate : String
    typedUpdate =
      if options.withTypes then
        toTypedUpdate ("update" ++ String.toTitleCase f) f t
      else
        ""

    genericUpdate : String
    genericUpdate =
      if options.withGenerics && options.withTypes then
        toGenericUpdate ("updateGeneric" ++ String.toTitleCase f) f
      else if options.withGenerics then
        toGenericUpdate ("update" ++ String.toTitleCase f) f
      else
        ""

    typedGetter : String
    typedGetter =
      if options.withTypes && options.withGetters then
        toTypedGetter ("get" ++ String.toTitleCase f) f t
      else 
        ""

    genericGetter : String
    genericGetter =
      if options.withGenerics && options.withTypes && options.withGetters then
        toGenericGetter ("getGeneric" ++ String.toTitleCase f) f
      else if options.withGenerics && options.withGetters then
        toGenericGetter ("get" ++ String.toTitleCase f) f
      else
        ""

    typedSetter : String
    typedSetter =
      if options.withTypes && options.withSetters then
        toTypedSetter ("set" ++ String.toTitleCase f) f t
      else 
        ""

    genericSetter : String
    genericSetter =
      if options.withGenerics && options.withTypes && options.withSetters then
        toGenericSetter ("setGeneric" ++ String.toTitleCase f) f
      else if options.withGenerics && options.withSetters then
        toGenericGetter ("set" ++ String.toTitleCase f) f
      else
        ""

    all : List String
    all =
      [ typedUpdate, genericUpdate, typedGetter, genericGetter, typedSetter, genericSetter ]

  in
  List.filter ((/=) "") all
    |> String.join "\n\n"

--
toTypedUpdate : String -> String -> String -> String
toTypedUpdate n f t =
  String.join "\n"
    [ n ++ " : (" ++ t ++ " -> " ++ t ++ ") -> { r | " ++ f ++ " : " ++ t ++ " } -> { r | " ++ f ++ " : " ++ t ++ " }"
    , n ++ " f ({ " ++ f ++ " } as r) ="
    , "  { r | " ++ f ++ " = f " ++ f ++ " }"
    ]

--
toGenericUpdate : String -> String -> String
toGenericUpdate n f =
  String.join "\n"
    [ n ++ " : (a -> a) -> { r | " ++ f ++ " : a } -> { r | " ++ f ++ " : a }"
    , n ++ " f ({ " ++ f ++ " } as r) ="
    , "  { r | " ++ f ++ " = f " ++ f ++ " }"
    ]

--
toTypedGetter : String -> String -> String -> String
toTypedGetter n f t =
  String.join "\n"
    [ n ++ " : { r | " ++ f ++ " : " ++ t ++ " } -> " ++ t
    , n ++ " { " ++ f ++ " } ="
    , "  " ++ f
    ]

--
toGenericGetter : String -> String -> String
toGenericGetter n f =
  String.join "\n"
    [ n ++ " : { r | " ++ f ++ " : a } -> a"
    , n ++ " { " ++ f ++ " } ="
    , "  " ++ f
    ]

--
toTypedSetter : String -> String -> String -> String
toTypedSetter n f t =
  String.join "\n"
    [ n ++ " : " ++ t ++ " -> { r | " ++ f ++ " : " ++ t ++ " } -> { r | " ++ f ++ " : " ++ t ++ " }"
    , n ++ " v ({ " ++ f ++ " } as r) ="
    , "  { r | " ++ f ++ " = v }"
    ]

--
toGenericSetter : String -> String -> String
toGenericSetter n f =
  String.join "\n"
    [ n ++ " : a -> { r | " ++ f ++ " : a } -> { r | " ++ f ++ " : a }"
    , n ++ " v ({ " ++ f ++ " } as r) ="
    , "  { r | " ++ f ++ " = v }"
    ]