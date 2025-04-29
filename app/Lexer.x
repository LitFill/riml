{
module Lexer where

import Token
import Data.Text qualified as T (pack, drop)
}

%wrapper "basic"

tokens :-
    [\ \t\n\r]+       ;                                      -- Mengabaikan whitespace
    [a-zA-Z0-9\-_\:]+ { \s -> TIdent $ T.pack s }            -- untuk nama tanpa tanda kutip
    \"[^\"]*\"        { \s -> TString $ T.pack s }           -- untuk String bertanda kutip
    \![^\n\r]*        { \s -> TComment $ T.drop 1 $ pack s } -- line comment
    "."               { \_ -> TDot }                         -- Titik
    "#"               { \_ -> THash }                        -- Hash
    "="               { \_ -> TEq }                          -- Sama dengan
    "{"               { \_ -> TLbrace }                      -- Kurung kurawal buka
    "}"               { \_ -> TRbrace }                      -- Kurung kurawal tutup
    ";"               { \_ -> TSemicolon }                   -- Titik Koma

{
}

