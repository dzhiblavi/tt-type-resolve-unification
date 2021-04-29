{
module Parser where

import Grammar
import Lexer
import Data.List (intercalate)
}

%name      parseExpr
%tokentype { Token }
%error     { parseError }
%monad     { Either String }{ >>= }{ return }

%token VAR    { VarT $$ }
%token LEFT   { LeftT   }
%token RIGHT  { RightT  }
%token DOT    { DotT    }
%token LAMBDA { LambdaT }

%%

Expr    
  : LAMBDA VAR DOT Expr     { (Var $2) :.: $4          }
  | App LAMBDA VAR DOT Expr { $1 :-: ((Var $3) :.: $5) }
  | App                     { $1 }

App
  : App Atom { $1 :-: $2 }
  | Atom     { $1        }

Atom
  : LEFT Expr RIGHT { $2 }
  | VAR             { V $ Var $1 }

{
parseError :: [Token] -> a
parseError tokens = error $ "Parse error: [" ++ intercalate ", " (map show tokens) ++ "]"
}

