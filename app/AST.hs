{-# LANGUAGE OverloadedStrings #-}
module AST where

import Data.Map  (Map)
import Data.Text (Text)

import Data.Map qualified as Map
import Data.Text qualified as T

data HtmlNode
    = Element
        { elTag      :: Text
        , elClasses  :: [Text]
        , elId       :: Maybe Text
        , elAttrs    :: Map Text Text
        , elChildren :: [HtmlNode]
        }
    | TextNode Text
    deriving (Show, Eq)

render :: HtmlNode -> Text
render (TextNode t) = f . f $ t where f = T.reverse . T.drop 1
render (Element et cls mei eas cs) =
    "<" <> et <> attrs <> ">" <> children <> "</" <> et <> ">"
  where
    attrPairs = [ ("class", T.intercalate " " cls) | not (null cls) ] <>
                [ ("id", i) | Just i <- [mei], not (T.null i) ] <>
                Map.toList eas
    attrStr = T.intercalate " " $ map (\(k,v) -> k <> "=" <> v) attrPairs
    attrs = if null attrPairs then "" else " " <> attrStr
    children = T.concat $ map render cs
