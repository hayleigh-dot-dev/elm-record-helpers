module Main exposing
  ( ..
  )

-- Imports ---------------------------------------------------------------------
import Browser
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Field exposing (Field)

-- Main ------------------------------------------------------------------------
main : Program Flags Model Msg
main =
  Browser.element
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }

-- Model -----------------------------------------------------------------------
--
type alias Flags =
  ()

--
type alias Model =
  { input : String
  , options : Field.Options
  , output : List Field
  }

--
init : Flags -> ( Model, Cmd Msg )
init flags =
  let
    input : String
    input =
      String.join "\n"
        [ "type alias Model ="
        , "  { count : Int"
        , "  }"
        ]
    
    options : Field.Options
    options =
      { withTypes = True
      , withGenerics = False
      , withGetters = False
      , withSetters = True
      }
  in
  withCmd Cmd.none <|
    { input = input
    , options = options
    , output = Field.fromString input
    }

-- Update ----------------------------------------------------------------------
--
type Msg
  = OnInput String
  | ToggleOption String

--
withCmd : Cmd Msg -> Model -> ( Model, Cmd Msg )
withCmd cmd model =
  ( model, cmd )

--
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    OnInput input ->
      withCmd Cmd.none <|
        { model
        | input = input
        , output = Field.fromString input
        }

    ToggleOption option ->
      case option of
        "withTypes" ->
          withCmd Cmd.none <|
            { model
            | options = updateWithTypes not model.options
            }

        "withGenerics" ->
          withCmd Cmd.none <|
            { model
            | options = updateWithGenerics not model.options
            }   

        "withGetters" ->
          withCmd Cmd.none <|
            { model
            | options = updateWithGetters not model.options
            }

        "withSetters" ->
          withCmd Cmd.none <|
            { model
            | options = updateWithSetters not model.options
            }   

        _ ->
          withCmd Cmd.none <|
            model    

--
updateWithGenerics : (Bool -> Bool) -> { r | withGenerics : Bool } -> { r | withGenerics : Bool }
updateWithGenerics f ({ withGenerics } as r) =
  { r | withGenerics = f withGenerics }

--
updateWithTypes : (Bool -> Bool) -> { r | withTypes : Bool } -> { r | withTypes : Bool }
updateWithTypes f ({ withTypes } as r) =
  { r | withTypes = f withTypes }

--
updateWithGetters : (Bool -> Bool) -> { r | withGetters : Bool } -> { r | withGetters : Bool }
updateWithGetters f ({ withGetters } as r) =
  { r | withGetters = f withGetters }

--
updateWithSetters : (Bool -> Bool) -> { r | withSetters : Bool } -> { r | withSetters : Bool }
updateWithSetters f ({ withSetters } as r) =
  { r | withSetters = f withSetters }

-- View ------------------------------------------------------------------------
--
view : Model -> Html Msg
view model =
  let
    title : String
    title =
      "elm-generate-updates"

    description : String
    description =
      String.join " "
        [ "This is a handy little utility to generate update functions for fields"
        , "in a record. Currently there is special syntax for record field getters"
        , "using .field but there is no setter equivalent, leading to a lot"
        , "of boilerplate when updating records. This is most felt in the update"
        , "function when updating parts of the model."
        ]

    output : String
    output =
      List.map (Field.toString model.options) model.output
        |> String.join "\n\n\n"

  in
  H.div
    [ A.class "container mx-auto"]
    [ H.section 
      []
      [ H.h1
        [ A.class "text-4xl" ]
        [ H.text title ]
      , H.p
        [ A.class "text-lg" ]
        [ H.text description ]
      ]
    , H.main_
      [ A.class "my-4" ]
      [ H.textarea
        [ A.attribute "data-input" ""
        , A.class "outline-none bg-gray-800 border-r-8 focus:border-purple-500 font-mono h-full mr-4 p-8 resize-none rounded-lg rounded-r-none text-white w-full" 
        , A.value model.input
        , E.onInput OnInput
        ] []
      , H.textarea
        [ A.attribute "data-output" ""
        , A.class "outline-none bg-gray-800 border-l-8 focus:border-purple-500 font-mono h-full ml-4 whitespace-pre p-8 resize-none rounded-lg rounded-l-none text-white w-full" 
        , A.value output
        , A.readonly True
        ] []
      ]
    , H.section 
      [ A.class "my-4" ]
      [ H.h2
        [ A.class "text-2xl" ]
        [ H.text "Options" ]
      , H.fieldset
        []
        [ H.label
          [ A.class "block" ]
          [ H.input
            [ A.type_ "checkbox" 
            , A.class "mr-2"
            , A.checked model.options.withTypes
            , E.onClick (ToggleOption "withTypes")
            ] []
          , H.text "Types"
          ]
        , H.label
          [ A.class "block" ]
          [ H.input
            [ A.type_ "checkbox"
            , A.class "mr-2" 
            , A.checked model.options.withGenerics
            , E.onClick (ToggleOption "withGenerics")
            ] []
          , H.text "Generics"
          ]
        , H.label
          [ A.class "block" ]
          [ H.input
            [ A.type_ "checkbox"
            , A.class "mr-2" 
            , A.checked model.options.withGetters
            , E.onClick (ToggleOption "withGetters")
            ] []
          , H.text "Getters"
          ]
        , H.label
          [ A.class "block" ]
          [ H.input
            [ A.type_ "checkbox"
            , A.class "mr-2" 
            , A.checked model.options.withSetters
            , E.onClick (ToggleOption "withSetters")
            ] []
          , H.text "Setters"
          ]
        ]
      ]
    ]

-- Subscriptions ---------------------------------------------------------------
--
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [
    ]