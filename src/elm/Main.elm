port module Main exposing
  ( main
  )

-- Imports ---------------------------------------------------------------------
import Browser
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Parser exposing (Parser, (|=), (|.))
import Regex
import Set exposing (Set)
import String.Extra as String
import Tuple.Extra as Tuple exposing (Tuple)

-- Ports -----------------------------------------------------------------------
port fromElm : String -> Cmd msg

type JavascriptMsg
  = CopyToClipboard

toJavascript : JavascriptMsg -> Cmd msg
toJavascript msg =
  case msg of
    CopyToClipboard ->
      fromElm "copy-to-clipboard"

-- Program ---------------------------------------------------------------------
---- Main ----------------------------------------------------------------------
main : Program () Model Msg
main =
  Browser.element
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }

---- Model ---------------------------------------------------------------------
type alias Model =
  { getters : Bool
  , setters : Bool
  , updates : Bool
  , input : String
  }

init : () -> Tuple Model (Cmd Msg)
init _ =
  Tuple.pairWith Cmd.none <|
    { getters = False
    , setters = True
    , updates = False
    , input =
        String.join "\n"
          [ "type alias Model = "
          , "  { count : Int"
          , "  }"
          ]
    }

---- Update --------------------------------------------------------------------
type Msg
  = OptionToggled Option
  | InputChanged String
  | OutputClicked

type Option
  = Getters
  | Setters
  | Updates

update : Msg -> Model -> Tuple Model (Cmd Msg)
update msg model =
  case msg of
    OptionToggled Getters ->
      updateGetters not model
        |> Tuple.pairWith Cmd.none

    OptionToggled Setters ->
      updateSetters not model
        |> Tuple.pairWith Cmd.none

    OptionToggled Updates ->
      updateUpdates not model
        |> Tuple.pairWith Cmd.none

    InputChanged input ->
      setInput input model
        |> Tuple.pairWith Cmd.none

    OutputClicked ->
      model
        |> Tuple.pairWith (toJavascript CopyToClipboard)


updateGetters : (Bool -> Bool) -> { r | getters : Bool } -> { r | getters : Bool }
updateGetters f ({ getters } as r) =
  { r | getters = f getters }

updateSetters : (Bool -> Bool) -> { r | setters : Bool } -> { r | setters : Bool }
updateSetters f ({ setters } as r) =
  { r | setters = f setters }

updateUpdates : (Bool -> Bool) -> { r | updates : Bool } -> { r | updates : Bool }
updateUpdates f ({ updates } as r) =
  { r | updates = f updates }

setInput : String -> { r | input : String } -> { r | input : String }
setInput val r =
  { r | input = val }

---- View ----------------------------------------------------------------------
view : Model -> Html Msg
view model =
  H.main_ [ A.class "h-screen flex flex-col p-4" ]
    -- About
    [ H.section [ A.class "mb-4" ]
      [ H.h1 [ A.class "text-2xl" ]
        [ H.text "elm-record-helpers" ]
      , H.p [ A.class "mb-2" ]
        [ H.text <| String.join " " 
          [ "This handy web app automagically generates some useful helper"
          , "functions for working with records. Elm currently only has special"
          , "syntax for accessing a record field. Setting a field to a new value"
          , "or updating a field by applying a function to the old value are"
          , "just a common tasks, however, which means we end up writing a lot"
          , "of repetitive boilerplate."
          ]
        ]
      , H.p []
        [ H.text <| String.join " " 
          [ "Simply write or copy and paste your record type definitions in the"
          , "left input. Then look at the generated helper functions on the"
          , "right. Clicking on the right text area will copy its contents to"
          , "your clipboard."
          ]
        ]
      ]
    -- Options
    , H.section []
      [ H.ul []
        [ H.li [ A.class "inline-block mr-6" ]
          [ H.label []
            [ H.input 
              [ A.class "mr-1"
              , A.type_ "checkbox"
              , A.checked model.getters
              , E.onClick (OptionToggled Getters) 
              ] []
            , H.text "Getters"
            ]
          ]
        , H.li [ A.class "inline-block mr-6" ]
          [ H.label []
            [ H.input
              [ A.class "mr-1"
              , A.type_ "checkbox"
              , A.checked model.setters
              , E.onClick (OptionToggled Setters) 
              ] []
            , H.text "Setters"
            ]
          ]
        , H.li [ A.class "inline-block mr-6" ]
          [ H.label []
            [ H.input
              [ A.class "mr-1"
              , A.type_ "checkbox"
              , A.checked model.updates
              , E.onClick (OptionToggled Updates) 
              ] []
            , H.text "Updates"
            ]
          ]
        ]
      ]
    -- Input
    , H.section [ A.class "flex-1 flex mt-4" ]
      [ H.textarea
          [ A.attribute "data-input" ""
          , A.class "flex-1 outline-none bg-gray-800 border-r-8 focus:border-purple-500 font-mono h-full mr-4 whitespace-pre p-8 resize-none rounded-lg rounded-r-none text-white w-full"
          , A.value model.input
          , E.onInput InputChanged
          ] []
      , H.textarea
        [ A.attribute "data-output" ""
        , A.class "flex-1 outline-none bg-gray-800 border-l-8 focus:border-purple-500 font-mono h-full ml-4 whitespace-pre p-8 resize-none rounded-lg rounded-l-none text-white w-full"
        , A.value <| parse (model.getters, model.setters, model.updates) model.input
        , A.readonly True
        , E.onClick OutputClicked
        ] []
      ]
    ]

---- Subscriptions -------------------------------------------------------------
subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.batch
    [
    ]

-- Record ----------------------------------------------------------------------
---- Types ---------------------------------------------------------------------
type alias Field =
  Tuple String String

---- Parsers -------------------------------------------------------------------
parse : (Bool, Bool, Bool) -> String -> String
parse (getters, setters, updates) input =
  Parser.run parser input
    |> Result.withDefault []
    |> List.concatMap (\field ->
      List.filterMap identity
        [ Just ("-- " ++ Tuple.first field ++ " " |> String.padRight 80 '-')
        , if getters then Just (fieldToGetter field) else Nothing
        , if setters then Just (fieldToSetter field) else Nothing
        , if updates then Just (fieldToUpdate field) else Nothing
        ]
    )
    |> String.join "\n\n"


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

fieldParser : Parser Field
fieldParser =
  Parser.succeed Tuple.pair
    |= Parser.getChompedString (
      Parser.succeed ()
        |. Parser.chompIf Char.isLower
        |. Parser.chompWhile (\c -> Char.isAlphaNum c || c == '_')
    )
    |. Parser.spaces
    |. Parser.token ":"
    |. Parser.spaces
    |= Parser.oneOf
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
      |> Parser.map (Tuple.map (trimInnerWhitespace >> String.trim))

---- Conversions ---------------------------------------------------------------
fieldToGetter : Field -> String
fieldToGetter (n, t) =
  let
    template : String
    template =
      String.join "\n"
        [ "get${fn-name} : { r | ${name} : ${type} } -> ${type}"
        , "get${fn-name} { ${name} } ="
        , "  ${name}"
        ]
  in
  interpolate template <|
    Set.fromList
      [ Tuple.pair "fn-name" (String.toTitleCase n)
      , Tuple.pair "name" n
      , Tuple.pair "type" t
      ]

fieldToSetter : Field -> String
fieldToSetter (n, t) =
  let
    template : String
    template =
      String.join "\n"
        [ "set${fn-name} : ${type} -> { r | ${name} : ${type} } -> { r | ${name} : ${type} }"
        , "set${fn-name} val r ="
        , "  { r | ${name} = val }"
        ]
  in
  interpolate template <|
    Set.fromList
      [ Tuple.pair "fn-name" (String.toTitleCase n)
      , Tuple.pair "name" n
      , Tuple.pair "type" t
      ]

fieldToUpdate : Field -> String
fieldToUpdate (n, t) =
  let
    template : String
    template =
      String.join "\n"
        [ "update${fn-name} : (${type} -> ${type}) -> { r | ${name} : ${type} } -> { r | ${name} : ${type} }"
        , "update${fn-name} f ({ ${name} } as r) ="
        , "  { r | ${name} = f ${name} }"
        ]
  in
  interpolate template <|
    Set.fromList
      [ Tuple.pair "fn-name" (String.toTitleCase n)
      , Tuple.pair "name" n
      , Tuple.pair "type" t
      ]

-- Utils -----------------------------------------------------------------------
interpolate : String -> Set (Tuple String String) -> String
interpolate template replacements =
  Set.foldl  (\(t, r) s -> String.replace ("${" ++ t ++ "}") r s) template replacements

trimInnerWhitespace : String -> String
trimInnerWhitespace string =
  Regex.fromStringWith { caseInsensitive = False, multiline = True } "\\s{2,}"
    |> Maybe.map (\r -> Regex.replace r (always " ") string)
    |> Maybe.withDefault string