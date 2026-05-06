module Syntax where

import Data.Matrix
import Data.Complex
import Data.Ratio

--Classical variables (bcs of OpenQASM3)
type CVar = String

--Configuration
type Conf = (C, StQ) -- depois mudar para StC para Mem

--quantum memory
type StQ = Matrix (Complex Double) --quantum state, represented by a density operator 
type QVar = String
type QVarList = [QVar]
type DoubProb = Double
type L = [(QVar, Loc)] -- linking function
type Loc = Int

--quantum operations
type Op = Matrix (Complex Double)-- operators are represented by matrices of complex numbers

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


--C expressions
data C = Skip 
       | U G QVarList
       | Meas QVar C C
       | Seq C C 
       | Whl QVar C
       | Par C C
       deriving (Show, Eq)



---CFG Ssyntax---
type NodeId = Int

-- Notation to insert in CFG nodes
data CFGNot = SSkip
             | UU G QVarList
             | MeasQ QVar
             deriving (Show, Eq)

data Node = Block NodeId [CFGNot]
          | MeasBlock NodeId [CFGNot]
          | WhlBlock NodeId [CFGNot]
          | Empty NodeId
          | Fork NodeId
          | Join NodeId
          deriving (Show, Eq)

data EdgeLabel = Uncond
               | TrueEdge -- measure 1
               | FalseEdge -- measure 0
               deriving (Show, Eq)

type Edge = (NodeId, EdgeLabel, NodeId)
type Graph = [Node]
type Edges = [Edge]          



