{
module Parser where

import Token
import AST
import Data.Map qualified as Map (fromList, empty)
}

%name        parse
%tokentype { Token      }
%error     { parseError }

%token
    IDENT   { TIdent $$   }
    STRING  { TString $$  }
    COMMENT { TComment $$ }
    '.'     { TDot        }
    '#'     { THash       }
    '='     { TEq         }
    '{'     { TLbrace     }
    '}'     { TRbrace     }
    ';'     { TSemicolon  }

%%

Document : Element { $1 }
         -- | Elements { Element "div" [] (Just "root") Map.empty $1 }

Elements : Element Elements { $1 : $2 }
         |                  { [] }

Element : IDENT Classes Id Attrs '{' Elements '}' { Element (Node $1 $2 $3 $4) $6 }
        | IDENT '{' Elements '}'                  { Element (Node $1 [] Nothing Map.empty) $3 }
        | IDENT '{' '}'                           { Element (Node $1 [] Nothing Map.empty) [] }
        | IDENT Classes Id Attrs ';'              { VoidNode (Node $1 $2 $3 $4) }
        | IDENT ';'                               { VoidNode (Node $1 [] Nothing Map.empty) }
        | COMMENT                                 { CommentNode $1 }
        | STRING                                  { TextNode $1 }

Classes : Class Classes { $1 : $2 }
        |               { [] }

Class : '.' IDENT { $2 }

Id : '#' IDENT { Just $2 }
   |           { Nothing }

Attrs : AttrsList { Map.fromList $1 }

AttrsList : AttrPair AttrsList { $1 : $2 }
          |                    { [] }

AttrPair : IDENT '=' STRING { ($1, $3) }

{
parseError :: [Token] -> a
parseError toks =
    error $
        "Kesalahan parsing: sintaks tidak valid. di: " ++
        show (take 10 toks)
}
