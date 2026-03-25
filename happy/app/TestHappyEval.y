-- this file aims to be an introduction to *happy* where we evaluate the elements that are Parsed
-- in this file, the precedence between operators is given by the format of the grammar
-- hence, it is not necessary to explicitly define the precedence of the operators

--Haskell code is written between curly braces  
{
module TestHappyEval where

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
-- the value of each production is a function from an environment p to a value
Exp : let var '=' Exp in Exp { \p -> $6 (($2, $4 p) : p) }
  | Exp1 { $1 }

Exp1 : Exp1 '+' Term { \p -> $1 p +  $3 p }
  | Exp1 '-' Term { \p -> $1 p -  $3 p }
  | Term { $1 }

Term : Term '*' Factor  { \p -> $1 p *  $3 p }
  | Term '/' Factor { \p -> $1 p `div`  $3 p }
  | Factor { $1 }

Factor : int { \p -> $1 }
  | var { \p -> case lookup $1 p of
            Nothing -> error "no var"
            Just i -> i}
  | '(' Exp ')' { $2 }


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
test :: String -> [(String, Int)] -> IO()
test s env = print $ calc (lexer s) env
     
}

  
