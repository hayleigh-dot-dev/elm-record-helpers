module Ui.Checkbox exposing
  ( Builder, builder
  , setChecked, setHandler, setLabel, addClass, addAttr, addLabelClass, addLabelAttr
  , toHtml
  )

-- Imports ---------------------------------------------------------------------
import Html as H exposing (Html, Attribute)
import Html.Attributes as A
import Html.Events as E

-- Types -----------------------------------------------------------------------
type alias Builder msg =
  { checked : Bool
  , handler : Maybe msg
  , label : Maybe String
  , classes : List String
  , attrs : List (Attribute msg)
  , labelClasses : List String
  , labelAttrs : List (Attribute msg)
  }

-- Buidler ---------------------------------------------------------------------
builder : Builder msg
builder =
  { checked = False
  , handler = Nothing
  , label = Nothing
  , classes = []
  , attrs = []
  , labelClasses = []
  , labelAttrs = []
  }

setChecked : Bool -> Builder msg -> Builder msg
setChecked checked checkbox =
  { checkbox | checked = checked }

setHandler : msg -> Builder msg -> Builder msg
setHandler handler checkbox =
  { checkbox | handler = Just handler }

setLabel : String -> Builder msg -> Builder msg
setLabel label checkbox = 
  { checkbox | label = Just label }

addClass : String -> Builder msg -> Builder msg
addClass class ({ classes } as checkbox) =
  { checkbox | classes = classes ++ [class] }

addAttr : Attribute msg -> Builder msg -> Builder msg
addAttr attr ({ attrs } as checkbox) =
  { checkbox | attrs = attrs ++ [attr] }

addLabelClass : String -> Builder msg -> Builder msg
addLabelClass class ({ labelClasses } as checkbox) =
  { checkbox | labelClasses = labelClasses ++ [class] }

addLabelAttr : Attribute msg -> Builder msg -> Builder msg
addLabelAttr attr ({ labelAttrs } as checkbox) =
  { checkbox | labelAttrs = labelAttrs ++ [attr] }

-- View ------------------------------------------------------------------------
toHtml : Builder msg -> Html msg
toHtml checkbox =
  let
    attrs =
      A.class (String.join " " checkbox.classes)
        :: A.type_ "checkbox"
        :: A.checked checkbox.checked
        :: (Maybe.map E.onClick checkbox.handler |> Maybe.withDefault (A.attribute "" ""))
        :: checkbox.attrs
  in
  case checkbox.label of
    Just label ->
      H.label ( A.class (String.join " " checkbox.labelClasses) :: checkbox.labelAttrs )
        [ H.input attrs []
        , H.text label
        ]

    Nothing ->
      H.input attrs []
