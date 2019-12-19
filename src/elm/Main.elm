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
  { headers : Bool
  , getters : Bool
  , setters : Bool
  , updates : Bool
  , monocle : Bool
  , copyOnClick : Bool
  , input : String
  }

init : () -> Tuple Model (Cmd Msg)
init _ =
  Tuple.pairWith Cmd.none <|
    { headers = False
    , getters = False
    , setters = True
    , updates = False
    , monocle = False
    , copyOnClick = False
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
  = Headers
  | Getters
  | Setters
  | Updates
  | Monocle
  | Copy

update : Msg -> Model -> Tuple Model (Cmd Msg)
update msg model =
  case msg of
    OptionToggled Headers ->
      updateHeaders not model
        |> Tuple.pairWith Cmd.none

    OptionToggled Getters ->
      updateGetters not model
        |> Tuple.pairWith Cmd.none

    OptionToggled Setters ->
      updateSetters not model
        |> Tuple.pairWith Cmd.none

    OptionToggled Updates ->
      updateUpdates not model
        |> Tuple.pairWith Cmd.none

    OptionToggled Monocle ->
      updateMonocle not model
        |> Tuple.pairWith Cmd.none

    OptionToggled Copy ->
      updateCopyOnClick not model
        |> Tuple.pairWith Cmd.none

    InputChanged input ->
      setInput input model
        |> Tuple.pairWith Cmd.none

    OutputClicked ->
      if model.copyOnClick then
        Tuple.pair model (toJavascript CopyToClipboard)
      else
        Tuple.pair model Cmd.none

updateHeaders : (Bool -> Bool) -> { r | headers : Bool } -> { r | headers : Bool }
updateHeaders f ({ headers } as r) =
  { r | headers = f headers }

updateGetters : (Bool -> Bool) -> { r | getters : Bool } -> { r | getters : Bool }
updateGetters f ({ getters } as r) =
  { r | getters = f getters }

updateSetters : (Bool -> Bool) -> { r | setters : Bool } -> { r | setters : Bool }
updateSetters f ({ setters } as r) =
  { r | setters = f setters }

updateUpdates : (Bool -> Bool) -> { r | updates : Bool } -> { r | updates : Bool }
updateUpdates f ({ updates } as r) =
  { r | updates = f updates }

updateMonocle : (Bool -> Bool) -> { r | monocle : Bool } -> { r | monocle : Bool }
updateMonocle f ({ monocle } as r) =
  { r | monocle = f monocle }

updateCopyOnClick : (Bool -> Bool) -> { r | copyOnClick : Bool } -> { r | copyOnClick : Bool }
updateCopyOnClick f ({ copyOnClick } as r) =
  { r | copyOnClick = f copyOnClick }

setInput : String -> { r | input : String } -> { r | input : String }
setInput val r =
  { r | input = val }

---- View ----------------------------------------------------------------------
view : Model -> Html Msg
view model =
  H.main_ [ A.class "h-screen flex flex-col p-4" ]
    -- About
    [ H.section [ A.class "mb-2" ]
      [ H.h1 [ A.class "text-2xl" ]
        [ H.text "elm-record-helpers" ]
      , H.p [ A.class "mb-2" ]
        [ H.text <| String.join " " 
          [ "This handy web app automagically generates some useful helper"
          , "functions for working with records. Elm currently only has special"
          , "syntax for accessing a record field. Setting a field to a new value"
          , "or updating a field by applying a function to the old value are"
          , "just as common tasks, however, which means we end up writing a lot"
          , "of repetitive boilerplate."
          ]
        ]
      , H.p []
        [ H.text <| String.join " " 
          [ "Simply write or copy and paste your record type definitions in the"
          , "left input. Then look at the generated helper functions on the"
          , "right. Clicking on the right text area will copy it's contents to"
          , "your clipboard."
          ]
        ]
      ]
    -- Options
    , H.section [ A.class "my-2" ]
      [ H.ul []
        [ H.li [ A.class "inline-block mr-8" ]
          [ H.label []
            [ H.input 
              [ A.class "mr-1"
              , A.type_ "checkbox"
              , A.checked model.headers
              , E.onClick (OptionToggled Headers) 
              ] []
            , H.text "Headers"
            ]
          ]
        , H.li [ A.class "inline-block mr-8" ]
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
        , H.li [ A.class "inline-block mr-8" ]
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
        , H.li [ A.class "inline-block mr-8" ]
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
        , H.li [ A.class "inline-block mr-8" ]
          [ H.label []
            [ H.input
              [ A.class "mr-1"
              , A.type_ "checkbox"
              , A.checked model.monocle
              , E.onClick (OptionToggled Monocle) 
              ] []
            , H.text "elm-monocle Lenses"
            ]
          ]
        , H.li [ A.class "inline-block mr-8" ]
          [ H.label []
            [ H.input
              [ A.class "mr-1"
              , A.type_ "checkbox"
              , A.checked model.copyOnClick
              , E.onClick (OptionToggled Copy) 
              ] []
            , H.text "Copy on click"
            ]
          ]
        ]
      ]
    -- Input
    , H.section [ A.class "flex-1 flex my-2" ]
      [ H.textarea
          [ A.attribute "data-input" ""
          , A.class "flex-1 outline-none bg-gray-800 border-r-8 focus:border-purple-500 font-mono h-full mr-4 whitespace-pre p-8 resize-none rounded-lg rounded-r-none text-white w-full"
          , A.value model.input
          , E.onInput InputChanged
          ] []
      , H.textarea
        [ A.attribute "data-output" ""
        , A.class "flex-1 outline-none bg-gray-800 border-l-8 focus:border-purple-500 font-mono h-full ml-4 whitespace-pre p-8 resize-none rounded-lg rounded-l-none text-white w-full"
        , A.value <| parse model model.input
        , A.readonly True
        , E.onClick OutputClicked
        ] []
      ]
    -- Font acknowledgement
    , H.section [ A.class "mt-4" ]
      [ H.p [ A.class "font-bold text-gray-500"]
        [ H.text "The font used on this page is 'Victor Mono' and can be found here: "
        , H.a 
          [ A.class "hover:underline text-gray-600 hover:text-gray-900"
          , A.href "https://rubjo.github.io/victor-mono/" ] 
          [ H.text "https://rubjo.github.io/victor-mono/" ]
        ]
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

type alias Options r = 
  { r
  | headers : Bool 
  , getters : Bool
  , setters : Bool
  , updates : Bool
  , monocle : Bool
  }

---- Parsers -------------------------------------------------------------------
parse : Options r -> String -> String
parse options input =
  Parser.run parser input
    |> Result.withDefault []
    |> List.concatMap (\field ->
      List.filterMap identity
        [ if options.headers then Just ("-- " ++ Tuple.first field ++ " " |> String.padRight 80 '-') else Nothing
        , if options.getters then Just (fieldToGetter field) else Nothing
        , if options.setters then Just (fieldToSetter field) else Nothing
        , if options.updates then Just (fieldToUpdate field) else Nothing
        , if options.monocle then Just (fieldToLens field) else Nothing
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

fieldToLens : Field -> String
fieldToLens (n, t) =
  let
    lensTemplate : String
    lensTemplate =
      String.join "\n"
        [ "${name}Lens : Lens { r | ${name} : ${type} } ${type}"
        , "${name}Lens ="
        , "  Lens .${name} (\\b a -> { a | ${name} = b })"
        ]

    optionalTemplate : String
    optionalTemplate =
      String.join "\n"
        [ "${name}Optional : Optional { r | ${name} : ${type} } ${just}"
        , "${name}Optional ="
        , "  Lens .${name} (\\b a -> { a | ${name} = Just b })"
        ]

  in
  if String.startsWith "Maybe " t then
    interpolate optionalTemplate <|
      Set.fromList
        [ Tuple.pair "name" n
        , Tuple.pair "type" t
        , Tuple.pair "just" (String.dropLeft 6 t)
        ]
  else
    interpolate lensTemplate <|
      Set.fromList
        [ Tuple.pair "name" n
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