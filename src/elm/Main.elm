port module Main exposing
  ( main
  )

-- Imports ---------------------------------------------------------------------
import Browser
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Json.Encode as Encode
import Tuple.Extra as Tuple exposing (Tuple)

import Data.Field

import Ui.Checkbox
import Ui.List
import Ui.Textarea

-- Ports -----------------------------------------------------------------------
port fromElm : Encode.Value -> Cmd msg

type JavascriptMsg
  = CopyToClipboard
  | UpdateUrl String

toJavascript : JavascriptMsg -> Cmd msg
toJavascript msg =
  case msg of
    CopyToClipboard ->
      fromElm <| Encode.object
        [ Tuple.pair "message" (Encode.string "copy-to-clipboard")
        ]

    UpdateUrl input ->
      fromElm <| Encode.object
        [ Tuple.pair "message" (Encode.string "update-url")
        , Tuple.pair "payload" (Encode.string input)
        ]

-- Main ----------------------------------------------------------------------
main : Program String Model Msg
main =
  Browser.element
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }

---- Model ---------------------------------------------------------------------
type alias Model =
  { options : Data.Field.Options
  , copyOnClick : Bool
  , input : String
  }

init : String -> Tuple Model (Cmd Msg)
init input =
  { options = Data.Field.defaults
  , copyOnClick = False
  , input = input
  } |> Tuple.pairWith (toJavascript (UpdateUrl input))

-- Update --------------------------------------------------------------------
type Msg
  = OptionToggled Option
  | InputChanged String
  | OutputClicked

type Option
  = Getters
  | Setters
  | Updates
  | Monocle
  | Copy

update : Msg -> Model -> Tuple Model (Cmd Msg)
update msg model =
  case msg of
    OptionToggled Getters ->
      { model | options = Data.Field.toggleGetters model.options }
        |> Tuple.pairWith Cmd.none

    OptionToggled Setters ->
      { model | options = Data.Field.toggleSetters model.options }
        |> Tuple.pairWith Cmd.none

    OptionToggled Updates ->
      { model | options = Data.Field.toggleUpdates model.options }
        |> Tuple.pairWith Cmd.none

    OptionToggled Monocle ->
      { model | options = Data.Field.toggleMonocle model.options }
        |> Tuple.pairWith Cmd.none

    OptionToggled Copy ->
      { model | copyOnClick = not model.copyOnClick }
        |> Tuple.pairWith Cmd.none

    InputChanged input ->
      { model | input = input }
        |> Tuple.pairWith (toJavascript (UpdateUrl input))

    OutputClicked ->
      if model.copyOnClick then
        Tuple.pair model (toJavascript CopyToClipboard)
      else
        Tuple.pair model Cmd.none

-- View ----------------------------------------------------------------------
view : Model -> Html Msg
view model =
  H.main_ [ A.class "p-4" ]
    [ infoSection
    , optionsSection model.options model.copyOnClick
    , inputSection model.options model.input
    , acknowledgementSection
    ]

infoSection : Html Msg
infoSection =
  H.section [ A.id "info", A.class "mb-4" ]
    [ H.h1 [ A.class "text-3xl font-bold" ]
      [ H.text "elm-record-helpers" ]
    , H.p [ A.class "" ]
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

optionsSection : Data.Field.Options -> Bool -> Html Msg
optionsSection options copyOnClick =
  H.section [ A.id "options", A.class "mb-4" ]
    [ H.h2 [ A.class "text-xl underline" ]
      [ H.text "Options" ]
    , Ui.List.builder
        |> Ui.List.addItemClass "inline-block mr-8"
        |> Ui.List.addItems (viewOptions
          [ ("Getters", options.getters, OptionToggled Getters)
          , ("Setters", options.setters, OptionToggled Setters)
          , ("Updates", options.updates, OptionToggled Updates)
          , ("Moncole", options.monocle, OptionToggled Monocle)
          ])
        |> Ui.List.toHtml
    , Ui.List.builder
        |> Ui.List.addItemClass "inline-block mr-8"
        |> Ui.List.addItems (viewOptions
          [ ("Copy-on-click", copyOnClick, OptionToggled Copy)
          ])
        |> Ui.List.toHtml
    ]

inputSection : Data.Field.Options -> String -> Html Msg
inputSection options input =
  H.section [ A.id "input", A.class "mb-4" ]
    [ viewCode
        |> Ui.Textarea.addClass "bg-gray-100 text-gray-900 mr-4"
        |> Ui.Textarea.setHandler InputChanged
        |> Ui.Textarea.setValue input
        |> Ui.Textarea.toHtml
    , viewCode
        |> Ui.Textarea.addClass "bg-gray-800 text-white ml-4"
        |> Ui.Textarea.addAttr (E.onClick OutputClicked)
        |> Ui.Textarea.addAttr (A.attribute "data-output" "")
        |> Ui.Textarea.setReadonly True
        |> Ui.Textarea.setValue (input
            |> Data.Field.parse
            |> List.map (Data.Field.toFunctions options)
            |> String.join "\n\n")
        |> Ui.Textarea.toHtml
    ]

acknowledgementSection : Html Msg
acknowledgementSection =
  H.section [ A.id "acknowledgements", A.class "mb-4" ]
    [ H.p [ A.class "font-bold text-gray-500"]
      [ H.text "The font used on this page is 'Victor Mono' and can be found here: "
      , H.a 
        [ A.class "hover:underline text-gray-600 hover:text-gray-900"
        , A.href "https://rubjo.github.io/victor-mono/" ] 
        [ H.text "https://rubjo.github.io/victor-mono/" ]
      ]
    ]

viewOptions : List (String, Bool, Msg) -> List (Html Msg)
viewOptions options =
  options |> List.map (\(label, checked, handler) ->
    Ui.Checkbox.builder
      |> Ui.Checkbox.setLabel label
      |> Ui.Checkbox.setHandler handler
      |> Ui.Checkbox.setChecked checked
      |> Ui.Checkbox.addClass "mr-1"
      |> Ui.Checkbox.addLabelClass (if checked then "underline" else "")
      |> Ui.Checkbox.toHtml  
  )

viewCode : Ui.Textarea.Builder msg
viewCode =
  Ui.Textarea.builder
    |> Ui.Textarea.addClass "flex-1"
    |> Ui.Textarea.addClass "border-2 focus:border-purple-500"
    |> Ui.Textarea.addClass "rounded-lg"
    |> Ui.Textarea.addClass "outline-none resize-none"
    |> Ui.Textarea.addClass "h-full w-full p-4"
    |> Ui.Textarea.addClass "font-mono whitespace-pre"

---- Subscriptions -------------------------------------------------------------
subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.batch
    [
    ]
