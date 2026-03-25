{
module Com where

import Data.Char

import Syntax
}


%name com C
%name prog Prog
%name aexp AExp
%name bexp BExp
%name sc SC


%tokentype { Token }

%error { parseError }

--Tokens
%token
  --Generic tokens
  int { TokenNum $$ }  --  $$ is a placeholder that represents the value of this token
  var { TokenVar $$ }
  '(' { TokenOP }
  ')' { TokenCP }
  '[' { TokenOB }
  ']' { TokenCB }
  '{' { TokenOCB }
  '}' { TokenCCB }
  ',' { TokenSemiColon }
  --Command tokens
  skip { TokenSkip }
  ":=" { TokenAss }
  ';' { TokenSeq }
  "if" { TokenIf }
  "else" { TokenElse }
  "then" { TokenThen }
  "while" { TokenWhile }
  "do" { TokenDo }
  --AE tokens
  '+' { TokenPlus }
  '-' { TokenMinus }
  '*' { TokenMult }
  '/' { TokenDiv }
  --BE tokens
  "tt" { TokenTrue }
  "ff" { TokenFalse }
  "and" { TokenAnd }
  "or" { TokenOr }
  '~' { TokenNot}
  '<' { TokenLess }
  '>' { TokenGreat }
  "<=" { TokenLE }
  ">=" { TokenGE }
  "==" { TokenEq }
  
--Precedence of the operators
%left '+' '-' -- left-associative with lower precedence
%left '*' '/' -- left-associative with higher precedence
%left "and" "or"
%nonassoc "==" '<' '>' ">=" "<="
%left NEG
%left "if" "while" 
%left "||"
%left ';'
%%

-- Production rules for the grammar
-- Production rules for programs
Prog :: { Prog } -- this is the default start symbol, meaning that com :: [Token] --> Prog
Prog : C ',' SC { ($1, $3) }
  | C { ($1, [])  }
  
-- Production rules for the commands  
C :: { C }
C : skip { Skip }
  | var ":=" AExp {Asg $1 $3 }
  | C ';' C { Seq $1 $3 }
  | "if" BExp "then" '{' C '}' "else" '{' C '}' {IfC $2 $5 $9}
  | "while" BExp "do" '{' C '}' {Whl $2 $5}
  | '(' C ')' { $2 }

-- Production rules for arithmetic expressions  
AExp :: { AExp }  
AExp : AExp '+' AExp { Plus $1 $3 }
  | AExp '-' AExp { Minus $1 $3 }
  | AExp '*' AExp  { Mult $1 $3 }
  | AExp '/' AExp { Div $1 $3 }
  | '(' AExp ')' { $2 }
  | '-' AExp %prec NEG { Negate $2 } 
  | int { Num $1 }
  | var { Var $1 }

-- Production rules for Boolean expressions
BExp :: { BExp }
BExp : "tt" { BTrue }
  | "ff" { BFalse }
  | '~' BExp { Not $2 }
  | BExp "and" BExp { And $1 $3 }
  | BExp "or" BExp { OrB $1 $3 }
  | AExp "==" AExp { Equ $1 $3 }
  | AExp '<' AExp { Less $1 $3 }
  | AExp '>' AExp { Gre $1 $3 }
  | AExp "<=" AExp { Leq $1 $3 }
  | AExp ">=" AExp { Geq $1 $3 }
  | '(' BExp ')' { $2 }

-- Production rules for classical states
SC :: { [(String, Int)] }
SC : int { [('c' : show(j), 0) | j <- [0 .. $1 -1]] }
  | '[' VarNameList ']' { [(var, 0) | var <- $2]  }
  | '[' Sts ']' { $2 }
  | {- empty -} { [] }

Sts :: { [(String, Int)] }
Sts : Sts ',' St { $3 : $1 }
  | St { [$1] }

St :: { (String, Int) }
St : '(' var ',' int ')' { ($2, $4) }

VarNameList :: { [String]  }
VarNameList : VarNameList ',' var { $3 : $1 }
  | var { [$1] }

{
--function that will be called whenever a parsing error occurs  
parseError :: [Token] -> a
parseError _ = error "Parse error"

--defining the Haskell data types used in the production rules above
-- these data types represents the parsed expressions

--declare the data type for the tokens
data Token = TokenSkip
           | TokenAss
           | TokenSeq
           | TokenNum Int
           | TokenVar String
           | TokenPlus
           | TokenMinus
           | TokenMult
           | TokenDiv
           | TokenOP
           | TokenCP
           | TokenOB
           | TokenCB
           | TokenOCB
           | TokenCCB
           | TokenSemiColon
           | TokenIf
           | TokenElse
           | TokenThen
           | TokenWhile
           | TokenDo
           | TokenTrue
           | TokenFalse
           | TokenAnd
           | TokenOr
           | TokenNot
           | TokenLess
           | TokenGreat
           | TokenLE
           | TokenGE
           | TokenEq
           deriving Show

--Definition of a simple lexer that returns this data structure
lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
  | isSpace c = lexer cs
  | isAlpha c = lexVar (c:cs)
  | isDigit c = lexNum (c:cs)
lexer (';':cs) = TokenSeq : lexer cs
lexer (':':'=':cs) = TokenAss : lexer cs
lexer ('+':cs) = TokenPlus : lexer cs
lexer ('-':cs) = TokenMinus : lexer cs
lexer ('*':cs) = TokenMult : lexer cs
lexer ('/':cs) = TokenDiv : lexer cs
lexer ('(':cs) = TokenOP : lexer cs
lexer (')':cs) = TokenCP : lexer cs
lexer ('[':cs) = TokenOB : lexer cs
lexer (']':cs) = TokenCB : lexer cs
lexer ('{':cs) = TokenOCB : lexer cs
lexer ('}':cs) = TokenCCB : lexer cs
lexer (',':cs) = TokenSemiColon : lexer cs
lexer ('~':cs) = TokenNot : lexer cs
lexer ('<':cs) = TokenLess : lexer cs
lexer ('>':cs) = TokenGreat : lexer cs
lexer ('<':'=':cs) = TokenLE : lexer cs
lexer ('>':'=':cs) = TokenGE : lexer cs
lexer ('=':'=':cs) = TokenEq : lexer cs

lexNum cs = TokenNum (read num) : lexer rest
      where (num,rest) = span isDigit cs

lexVar cs =
   case span isAlphaNum cs of
      ("skip",rest) -> TokenSkip : lexer rest
      ("if", rest) -> TokenIf : lexer rest
      ("then", rest) -> TokenThen : lexer rest
      ("else", rest) -> TokenElse : lexer rest
      ("while", rest) -> TokenWhile : lexer rest
      ("do", rest) -> TokenDo : lexer rest
      ("and", rest) -> TokenAnd : lexer rest
      ("or", rest) -> TokenOr : lexer rest
      ("tt", rest) -> TokenTrue : lexer rest
      ("ff", rest) -> TokenFalse : lexer rest
      (var,rest)   -> TokenVar var : lexer rest
      

-- function that takes some input, parses it and prints out the result
testCom :: String -> C
testCom s = com $ lexer s

testProg :: String -> (C, SC)
testProg s = let (c, st) = prog $ lexer s
             in (c, reverse st)

testAExp :: String -> AExp
testAExp s = aexp $ lexer s

testBExp :: String -> BExp
testBExp s = bexp $ lexer s

testSC :: String -> SC
testSC s = sc $ lexer s
}

