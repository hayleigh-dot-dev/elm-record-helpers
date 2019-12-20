module Ui.List exposing
  ( Builder, builder
  , addClass, addAttr, addItemClass, addItemAttr, addItem, addItems
  , toHtml
  )

-- Imports ---------------------------------------------------------------------
import Html as H exposing (Html, Attribute)
import Html.Attributes as A

-- Types -----------------------------------------------------------------------
type alias Builder msg =
  { classes : List String
  , attrs : List (Attribute msg)
  , itemClasses : List String
  , itemAttrs : List (Attribute msg)
  , items : List (Html msg)
  }

-- Buidler ---------------------------------------------------------------------
builder : Builder msg
builder =
  { classes = []
  , attrs = []
  , itemClasses = []
  , itemAttrs = []
  , items = []
  }

addClass : String -> Builder msg -> Builder msg
addClass class ({ classes } as list) =
  { list | classes = classes ++ [class] }

addAttr : Attribute msg -> Builder msg -> Builder msg
addAttr attr ({ attrs } as list) =
  { list | attrs = attrs ++ [attr] }

addItemClass : String -> Builder msg -> Builder msg
addItemClass class ({ itemClasses } as list) =
  { list | itemClasses = itemClasses ++ [class] }

addItemAttr : Attribute msg -> Builder msg -> Builder msg
addItemAttr attr ({ itemAttrs } as list) =
  { list | itemAttrs = itemAttrs ++ [attr] }

addItem : Html msg -> Builder msg -> Builder msg
addItem item ({ items } as list) =
  { list | items = items ++ [item] }

addItems : List (Html msg) -> Builder msg -> Builder msg
addItems items list =
  { list | items = list.items ++ items }

-- View ------------------------------------------------------------------------
toHtml : Builder msg -> Html msg
toHtml list =
  H.ul (A.class (String.join " " list.classes) :: list.attrs)
    <| List.map (itemToHtml list.itemClasses list.itemAttrs) list.items

itemToHtml : List String -> List (Attribute msg) -> Html msg -> Html msg
itemToHtml classes attrs item =
  H.li (A.class (String.join " " classes) :: attrs)
    [ item ] 