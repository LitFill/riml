module AST where

import Data.List (intercalate)
import Data.Map  (Map)

import Data.Map qualified as Map

data HtmlNode
    = Element
        { elTag      :: String
        , elClasses  :: [String]
        , elId       :: Maybe String
        , elAttrs    :: Map String String
        , elChildren :: [HtmlNode]
        }
    | TextNode String
    deriving (Show, Eq)

render :: HtmlNode -> String
render (TextNode t) = t
render (Element et cls mei eas cs) =
    "<" ++ et ++ attrs ++ ">" ++ children ++ "</" ++ et ++ ">"
  where
    attrPairs = [ ("class", show $ intercalate " " cls) | not (null cls) ] ++
                [ ("id", show i) | Just i <- [mei], not (null i) ] ++
                Map.toList eas
    attrStr = intercalate " " $ map (\(k,v) -> k ++ "=" ++ v) attrPairs
    attrs = if null attrPairs then "" else " " ++ attrStr
    children = concatMap render cs
