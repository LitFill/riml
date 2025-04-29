module Token where

import Data.Text (Text)

data Token
    = TIdent Text  -- div
    | TString Text -- "text"
    | TComment Text -- for line comment
    | TDot         -- .class
    | THash        -- #id
    | TEq          -- attr="value"
    | TLbrace      -- {
    | TRbrace      -- }
    | TSemicolon   -- ;

    deriving (Show, Eq)
