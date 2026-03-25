-- this file aims to be an introduction to *happy*
-- the elements that are parsed having in mind the precedence between the different operators

--Haskell code is written between curly braces  
{
module TestHappyEvalPrec where

import Data.Char
}

-- name of the parsing function that Happy will generate
-- calc : [Token] -> T, where T is the return type of the parser, determined by the production rules
%name calc 

-- type of tokens that the parser will accept
%tokentype { Token }

-- the name function called in the event of a parse error
%error { parseError }


-- this declares all the possible tokens
-- the symbols on the left are the tokens as they will be referred to in the rest of the grammar
-- to the right of each token enclosed in braces is a Haskell pattern that matches the token
-- The parsers expects to receive a stream of tokens
%token
  let { TokenLet }
  in  { TokenIn }
  int { TokenInt $$ }  --  $$ is a placeholder that represents the value of this token
  var { TokenVar $$ }
  '=' { TokenEq }
  '+' { TokenPlus  }
  '-' { TokenMinus  }
  '*' { TokenTimes }
  '/' { TokenDiv }
  '(' { TokenOB }
  ')' { TokenCB }

-- the precedence of the operators is placed before the production rules
-- the earlier a precedence is defined, the lower it is
-- there is also the precedence %nonassoc, which indicates that the operators must not be used together
%right in -- right-associative
%left '+' '-' -- left-associative with lower precedence
%left '*' '/' -- left-associative with higher precedence
%left NEG
%%
-- Production rules for the grammar
Exp :: { Exp }  
Exp : let var '=' Exp in Exp { Let $2 $4 $6 }
  | Exp '+' Exp { Plus $1 $3 }
  | Exp '-' Exp { Minus $1 $3 }
  | Exp '*' Exp  { Times $1 $3 }
  | Exp '/' Exp { Div $1 $3 }
  | '(' Exp ')' { Brack $2 }
  | '-' Exp %prec NEG { Negate $2 } 
  | int { Int $1 }
  | var { Var $1 }

{-
note that the token NEG does not need to appear in a %token directive;
the prefix negation rule has a %prec Neg directive attached, which overrides the default precedence
for the rule (which would normally be the precedence of ´-´) with the precedence of NEG
-}


{
--function that will be called whenever a parsing error occurs  
parseError :: [Token] -> a
parseError _ = error "Parse error"

--defining the Haskell data types used in the production rules above
-- these data types represents the parsed expressions
data Exp = Let String Exp Exp
         | Exp Exp
         | Plus Exp Exp
         | Minus Exp Exp
         | Times Exp Exp
         | Div Exp Exp
         | Int Int
         | Var String
         | Negate Exp
         | Brack Exp
         deriving Show

--declare the data type for the tokens
data Token = TokenLet
           | TokenIn
           | TokenInt Int
           | TokenVar String
           | TokenEq
           | TokenPlus
           | TokenMinus
           | TokenTimes
           | TokenDiv
           | TokenOB
           | TokenCB
           deriving Show

--Definition of a simple lexer that returns this data structure
lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
  | isSpace c = lexer cs
  | isAlpha c = lexVar (c:cs)
  | isDigit c = lexNum (c:cs)
lexer ('=':cs) = TokenEq : lexer cs
lexer ('+':cs) = TokenPlus : lexer cs
lexer ('-':cs) = TokenMinus : lexer cs
lexer ('*':cs) = TokenTimes : lexer cs
lexer ('/':cs) = TokenDiv : lexer cs
lexer ('(':cs) = TokenOB : lexer cs
lexer (')':cs) = TokenCB : lexer cs

lexNum cs = TokenInt (read num) : lexer rest
      where (num,rest) = span isDigit cs

lexVar cs =
   case span isAlpha cs of
      ("let",rest) -> TokenLet : lexer rest
      ("in",rest)  -> TokenIn : lexer rest
      (var,rest)   -> TokenVar var : lexer rest

-- function that takes some input, parses it and prints out the result
test :: String -> IO()
test s = print $ calc $ lexer s
     
}

  
