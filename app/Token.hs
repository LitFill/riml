module Token where

data Token
    = TIdent String -- div
    | TString String -- "text"
    | TDot -- .class
    | THash -- #id
    | TEq -- attr="value"
    | TLbrace -- {
    | TRbrace -- }

    deriving (Show, Eq)
