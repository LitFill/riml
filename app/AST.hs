{-# LANGUAGE OverloadedStrings #-}

module AST where

import Data.Map  (Map)
import Data.Text (Text)

import Prettyprinter
import Prettyprinter.Render.String (renderString)

import Data.Map  qualified as Map
import Data.Text qualified as T

data Node = Node
    { nodeTag      :: Text
    , nodeClasses  :: [Text]
    , nodeId       :: Maybe Text
    , nodeAttrs    :: Map Text Text
    } deriving (Show, Eq)

data HtmlNode
    = TextNode Text
    | VoidNode Node
    | Element
        { elNode     :: Node
        , elChildren :: [HtmlNode]
        }
    deriving (Show, Eq)

instance Pretty HtmlNode where
    pretty (TextNode t) = pretty.f.f $ t
      where f = T.reverse . T.drop 1
    pretty (VoidNode (Node tag classes mid attrs)) =
        langle
        <> pretty tag
        <> ppAttrs classes mid attrs
        <> rangle
    pretty (Element (Node tag classes mid attrs) children) =
        let
            allAttrs = ppAttrs classes mid attrs
            opentag  = langle <> pretty tag <> allAttrs   <> rangle
            closetag = langle <> slash      <> pretty tag <> rangle
         in
         if   null children
         then group $ opentag <> closetag
         else vsep [ opentag
                  , indent 4 . vsep $ map pretty children
                  , closetag ]

ppAttrs :: [Text] -> Maybe Text -> Map Text Text -> Doc ann
ppAttrs classes mid attrs =
    let
        clsAttr  = [ ("class", T.show (T.intercalate " " classes))
                   | not . null $ classes ]
        idAttr   = [ ("id",    T.show i)
                   | Just i <- [mid]
                   , not . T.null $ i ]
        allAttrs = clsAttr ++ idAttr ++ Map.toList attrs
     in
     if null allAttrs
        then mempty
        else space <> hsep (map ppAttr allAttrs)
  where
    ppAttr (k, v) = pretty k <> equals <> pretty v

prettyRender :: HtmlNode -> String
prettyRender node =
    let layoutOpts =
            LayoutOptions
                { layoutPageWidth = AvailablePerLine 80 1.0 }
     in renderString
        . layoutPretty layoutOpts
        $ pretty node

render :: HtmlNode -> Text
render (TextNode t) =
    f . f $ t where f = T.reverse . T.drop 1
render (VoidNode (Node et cls mei eas)) =
    "<" <> et <> attrs <> ">"
  where
    attrPairs =
        [ ("class", T.show $ T.intercalate " " cls)
        | not (null cls)
        ] <>
        [ ("id", T.show i)
        | Just i <- [mei]
        , not (T.null i)
        ] <> Map.toList eas

    pairstrfy (k, v) = k <> "=" <> v
    attrStr =
        T.intercalate " "
        $ map pairstrfy attrPairs
    attrs =
        if null attrPairs
            then ""
            else " " <> attrStr
render (Element (Node et cls mei eas) cs) =
    "<" <> et <> attrs <> ">"
    <> children
    <> "</" <> et <> ">"
  where
    attrPairs =
        [ ("class", T.show $ T.intercalate " " cls)
        | not (null cls)
        ] <>
        [ ("id", T.show i)
        | Just i <- [mei]
        , not (T.null i)
        ] <> Map.toList eas

    pairstrfy (k, v) = k <> "=" <> v
    attrStr =
        T.intercalate " "
        $ map pairstrfy attrPairs
    attrs =
        if null attrPairs
            then ""
            else " " <> attrStr
    children = T.concat $ map render cs
