module Ui.Textarea exposing
  ( Builder, builder
  , addClass, addAttr, setReadonly, setHandler, setValue
  , toHtml
  )

-- Imports ---------------------------------------------------------------------
import Html as H exposing (Html, Attribute)
import Html.Attributes as A
import Html.Events as E

-- Types -----------------------------------------------------------------------
type alias Builder msg =
  { classes : List String
  , attrs : List (Attribute msg)
  , readonly : Bool
  , handler : Maybe (String -> msg)
  , value : String
  }

-- Buidler ---------------------------------------------------------------------
builder : Builder msg
builder =
  { classes = []
  , attrs = [] 
  , readonly = False
  , handler = Nothing
  , value = ""
  }

addClass : String -> Builder msg -> Builder msg
addClass class ({ classes } as textarea) =
  { textarea | classes = classes ++ [class] }

addAttr : Attribute msg -> Builder msg -> Builder msg
addAttr attr ({ attrs } as textarea) =
  { textarea | attrs = attrs ++ [attr] }

setReadonly : Bool -> Builder msg -> Builder msg
setReadonly readonly textarea =
  { textarea | readonly = readonly }

setHandler : (String -> msg) -> Builder msg -> Builder msg
setHandler handler textarea =
  { textarea | handler = Just handler }

setValue : String -> Builder msg -> Builder msg
setValue value textarea =
  { textarea | value = value }

-- View ------------------------------------------------------------------------
toHtml : Builder msg -> Html msg
toHtml textarea =
  let
    attrs =
      A.class (String.join " " textarea.classes)
        :: (Maybe.map E.onInput textarea.handler |> Maybe.withDefault (A.class ""))
        :: A.readonly textarea.readonly
        :: A.value textarea.value
        :: textarea.attrs
  in
  H.textarea attrs []