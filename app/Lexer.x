{
module Lexer where
import Token
}

%wrapper "basic"

tokens :-
    [\ \t\n\r]+     ;                   -- Mengabaikan whitespace
    [a-zA-Z0-9\-_]+ { \s -> TIdent s }  -- untuk nama tanpa tanda kutip
    \"[^\"]*\"      { \s -> TString s } -- untuk String bertanda kutip
    "."             { \_ -> TDot }      -- Titik
    "#"             { \_ -> THash }     -- Hash
    "="             { \_ -> TEq }       -- Sama dengan
    "{"             { \_ -> TLbrace }   -- Kurung kurawal buka
    "}"             { \_ -> TRbrace }   -- Kurung kurawal tutup

{
}

