module Syntax where

type SC = [(String, Int)]
type Prog = (C, SC)
type Var = String

data C = Skip
       | Asg Var AExp
       | Seq C C
       | IfC BExp C C
       | Whl BExp C
       deriving (Show, Eq)

data AExp = Plus AExp AExp
         | Minus AExp AExp
         | Mult AExp AExp
         | Div AExp AExp
         | Num Int
         | Var String
         | Negate AExp
         deriving Eq

instance Show AExp where
  show (Num n) = show n
  show (Var x) = x
  show (Plus a b)  = "(" ++ show a ++ " + " ++ show b ++ ")"
  show (Minus a b) = "(" ++ show a ++ " - " ++ show b ++ ")"
  show (Mult a b)  = "(" ++ show a ++ " * " ++ show b ++ ")"
  show (Div a b)   = "(" ++ show a ++ " / " ++ show b ++ ")"
  show (Negate a)  = "(-" ++ show a ++ ")"

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
          deriving Eq

instance Show BExp where
  show BTrue  = "true"
  show BFalse = "false"
  show (Not b)      = "not (" ++ show b ++ ")"
  show (And b1 b2)  = "(" ++ show b1 ++ " && " ++ show b2 ++ ")"
  show (OrB b1 b2)  = "(" ++ show b1 ++ " || " ++ show b2 ++ ")"
  show (Equ a b)    = "(" ++ show a ++ " == " ++ show b ++ ")"
  show (Leq a b)    = "(" ++ show a ++ " <= " ++ show b ++ ")"
  show (Geq a b)    = "(" ++ show a ++ " >= " ++ show b ++ ")"
  show (Less a b)   = "(" ++ show a ++ " < "  ++ show b ++ ")"
  show (Gre a b)    = "(" ++ show a ++ " > "  ++ show b ++ ")"

type NodeId = Int

-- Notation to insert in CFG nodes
data CFGNot = SSkip
             | SAsg Var AExp
             | Phi Var [Var]
             | BE BExp
             deriving (Show, Eq)

data Node = Entry NodeId
          | Exit NodeId
          | Block NodeId [CFGNot]
          | Test NodeId [CFGNot]
          | Join NodeId
          | Empty NodeId
          deriving (Show, Eq)

data EdgeLabel = Uncond
               | TrueEdge
               | FalseEdge
               deriving (Show, Eq)

type Edge = (NodeId, EdgeLabel, NodeId)
type Graph = [Node]
type Edges = [Edge]          
