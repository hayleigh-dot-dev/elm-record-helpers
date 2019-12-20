module Data.Field exposing
  ( Field
  , Options, defaults, toggleGetters, toggleSetters, toggleUpdates, toggleMonocle
  , toFunctions
  , parse
  )

-- Imports ---------------------------------------------------------------------
import Parser exposing (Parser, (|=), (|.))
import Set
import String.Extra as String
import Tuple.Extra as Tuple exposing (Tuple)
import Utils.String as String

-- Types -----------------------------------------------------------------------
{- -}
type alias Field
  = Tuple String String

type alias Options =
  { getters : Bool
  , setters : Bool
  , updates : Bool
  , monocle : Bool
  }

-- Constants -------------------------------------------------------------------
defaults : Options
defaults =
  { getters = False
  , setters = True
  , updates = False
  , monocle = False
  }

-- Options ---------------------------------------------------------------------
toggleGetters : { r | getters : Bool } -> { r | getters : Bool }
toggleGetters ({ getters } as r) =
  { r | getters = not getters }

toggleSetters : { r | setters : Bool } -> { r | setters : Bool }
toggleSetters ({ setters } as r) =
  { r | setters = not setters }

toggleUpdates : { r | updates : Bool } -> { r | updates : Bool }
toggleUpdates ({ updates } as r) =
  { r | updates = not updates }

toggleMonocle : { r | monocle : Bool } -> { r | monocle : Bool }
toggleMonocle ({ monocle } as r)=
  { r | monocle = not monocle }

-- Function strings ------------------------------------------------------------
toFunctions : Options -> Field -> String
toFunctions options field =
  String.join "\n\n"
    [ if options.getters then toGetFunction field else ""
    , if options.setters then toSetFunction field else ""
    , if options.updates then toUpdateFunction field else ""
    , if options.monocle then toMonocleLens field else ""
    ]
    |> String.trim

toGetFunction : Field -> String
toGetFunction (fName, fType) =
  let
    template =
      String.join "\n"
        [ "get${fn-name} : { r | ${name} : ${type} } -> ${type}"
        , "get${fn-name} { ${name} } ="
        , "  ${name}"
        ]
  in
  String.interpolate template <|
    Set.fromList
      [ Tuple.pair "fn-name" (String.toTitleCase fName)
      , Tuple.pair "name" fName
      , Tuple.pair "type" fType
      ]

toSetFunction : Field -> String
toSetFunction (fName, fType) =
  let
    template =
      String.join "\n"
        [ "set${fn-name} : ${type} -> { r | ${name} : ${type} } -> { r | ${name} : ${type} }"
        , "set${fn-name} val r ="
        , "  { r | ${name} = val }"
        ]
  in
  String.interpolate template <|
    Set.fromList
      [ Tuple.pair "fn-name" (String.toTitleCase fName)
      , Tuple.pair "name" fName
      , Tuple.pair "type" fType
      ]

toUpdateFunction : Field -> String
toUpdateFunction (fName, fType) =
  let
    template =
      String.join "\n"
        [ "update${fn-name} : (${type} -> ${type}) -> { r | ${name} : ${type} } -> { r | ${name} : ${type} }"
        , "update${fn-name} f ({ ${name} } as r) ="
        , "  { r | ${name} = f ${name} }"
        ]
  in
  String.interpolate template <|
    Set.fromList
      [ Tuple.pair "fn-name" (String.toTitleCase fName)
      , Tuple.pair "name" fName
      , Tuple.pair "type" fType
      ]

toMonocleLens : Field -> String
toMonocleLens (fName, fType) =
  let
    lensTemplate =
      String.join "\n"
        [ "${name}Lens : Lens { r | ${name} : ${type} } ${type}"
        , "${name}Lens ="
        , "  Lens .${name} (\\b a -> { a | ${name} = b })"
        ]

    optionalTemplate =
      String.join "\n"
        [ "${name}Optional : Optional { r | ${name} : ${type} } ${just}"
        , "${name}Optional ="
        , "  Lens .${name} (\\b a -> { a | ${name} = Just b })"
        ]
  in
  if String.startsWith "Maybe " fType then
    String.interpolate optionalTemplate <|
      Set.fromList
        [ Tuple.pair "name" fName
        , Tuple.pair "type" fType
        , Tuple.pair "just" (String.dropLeft 6 fType)
        ]
  else
    String.interpolate lensTemplate <|
      Set.fromList
        [ Tuple.pair "name" fName
        , Tuple.pair "type" fType
        ]

-- Parsers ---------------------------------------------------------------------
parse : String -> List Field
parse input =
  Parser.run parser input
    |> Result.withDefault []

{- -}
parser : Parser (List Field)
parser =
  Parser.loop [] (\fields ->
    Parser.oneOf
      [ Parser.succeed (\record -> Parser.Loop (fields ++ record))
          |= recordParser
          |. Parser.spaces
      , Parser.succeed (\_ -> Parser.Done fields)
          |= Parser.end
      ]
  )

{- -}
recordParser : Parser (List Field)
recordParser =
  Parser.succeed identity
    |. Parser.chompUntil "{"
    |= Parser.sequence
      { start = "{"
      , separator = ","
      , end = "}"
      , spaces = Parser.spaces
      , item = fieldParser
      , trailing = Parser.Forbidden
      }

{- -}
fieldParser : Parser Field
fieldParser =
  Parser.succeed Tuple.pair
    |= fieldNameParser
    |. Parser.spaces
    |. Parser.token ":"
    |. Parser.spaces
    |= fieldTypeParser
    |> Parser.map (Tuple.map (String.trimInnerWhitespace >> String.trim))

{- -}
fieldNameParser : Parser String
fieldNameParser =
  Parser.getChompedString (
    Parser.succeed ()
      |. Parser.chompIf Char.isLower
      |. Parser.chompWhile (\c -> Char.isAlphaNum c || c == '_')
  )

{- -}
fieldTypeParser : Parser String
fieldTypeParser =
  Parser.oneOf
    [ Parser.backtrackable <| Parser.getChompedString (
        Parser.succeed ()
          |. Parser.chompIf Char.isAlpha
          |. Parser.chompWhile (\c -> c /= ',' && c /= '}')
      )
    , Parser.backtrackable <| Parser.getChompedString (
        Parser.succeed ()
          |. Parser.chompIf ((==) '(')
          |. Parser.chompWhile ((/=) ')')
          |. Parser.token ")"
      )
    , Parser.backtrackable <| Parser.getChompedString (
        Parser.succeed ()
          |. Parser.chompIf ((==) '{')
          |. Parser.chompWhile ((/=) '}')
          |. Parser.token "}"
      )
    ]
