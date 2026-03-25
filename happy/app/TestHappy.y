-- this file aims to be an introduction to *happy*
-- at this point we do not evaluate the elements that are parsed
-- this is going to be made in TestHappyEval, which will also contain precedences between the different operators

--Haskell code is written between curly braces  
{
module TestHappy where

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

%%
-- Production rules for the grammar
Exp : let var '=' Exp in Exp { Let $2 $4 $6 }
  | Exp1 { Exp1 $1 }

Exp1 : Exp1 '+' Term { Plus $1 $3 }
  | Exp1 '-' Term { Minus $1 $3 }
  | Term { Term $1 }

Term : Term '*' Factor  { Times $1 $3 }
  | Term '/' Factor { Div $1 $3 }
  | Factor { Factor $1 }

Factor : int { Int $1 }
  | var { Var $1 }
  | '(' Exp ')' { Brack $2 }


{
--function that will be called whenever a parsing error occurs  
parseError :: [Token] -> a
parseError _ = error "Parse error"

--defining the Haskell data types used in the production rules above
-- these data types represents the parsed expressions
data Exp = Let String Exp Exp
         | Exp1 Exp1
         deriving Show

data Exp1 = Plus Exp1 Term
          | Minus Exp1 Term
          | Term Term
          deriving Show

data Term = Times Term Factor
          | Div Term Factor
          | Factor Factor
          deriving Show

data Factor = Int Int
            | Var String
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

  
