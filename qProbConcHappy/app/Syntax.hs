module Syntax where

import Data.Matrix
import Data.Complex
import Data.Ratio

--Configuration
type ConfL = (C, LMem)
type Conf = (C, StC) -- depois mudar para StC para Mem

--classical memory
type StC = [(CVar, Value)]
type CVar = String
type Value = Double

--quantum memory
type StQ = Matrix (Complex Double) --quantum state, represented by a density operator 
type QVar = String
type QVarList = [QVar]
type Prob = Double
type L = [(QVar, Loc)] -- linking function
type Loc = Int

--memory
type Mem = (StC, StQ)
type LMem = (StC, L, StQ) -- has the linking function, useful for SQ

--quantum operations
type Op = Matrix (Complex Double)-- operators are represented by matrices of complex numbers

--ProbPath: (X x V(S+X))* x X ==> ([(X, V(S + X))], X)
-- X = (C,LMem)
-- V(S+X) = [(Either S X, Double)]
-- ([((C,LMem), [(Either LMem (C,LMem), Double)])], (C,LMem))
type ProbPath = ([(ConfL, [(Either LMem ConfL, Double)])], ConfL)

--AExp expressions
data AExp = Num Double
          | Var String
          | Pi
          | Plus AExp AExp
          | Minus AExp AExp
          | Mult AExp AExp
          | Div AExp AExp
          | Negate AExp
          | Sqrt AExp
          deriving (Show, Eq)

--BExp expressions
data BExp = BTrue
          | BFalse
          | Not BExp
          | And BExp BExp
          | OrB BExp BExp
          | Equ AExp AExp
          | Leq AExp AExp
          | Geq AExp AExp
          | Less AExp AExp
          | Gre AExp AExp
          deriving (Show, Eq)

--Gates considered in the language
data G = I
       | X
       | Y
       | Z
       | H
       | S
       | T
       | SWAP
       | CNOT
       | CZ
       | TOF
       | Ph AExp
       | CPh AExp
       | Umag2
       | Vmag3
       | UD String --added to support User Defined unitary gates
       deriving (Show, Eq)

-- --Arg data type, for the arguments of parameterized phase gates
-- data Arg = NumP Double
--          | PiP
--          | SqrtP Arg
--          | PlusP Arg Arg
--          | MinusP Arg Arg
--          | MultP Arg Arg
--          | DivP Arg Arg
--          deriving (Show, Eq)

--C expressions
data C = Skip 
       | Asg CVar AExp
       | Reset QVar
       | U G QVarList
       | Meas (CVar, QVar) 
       | Seq C C 
       | Or C C 
       | Par C C 
       | P Rational C C 
       | IfC BExp C C 
       | Whl BExp C
       | Await BExp C
       | Atom C
       deriving (Show, Eq)

