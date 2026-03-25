module Gates where

import Data.Matrix
import Data.Complex
import Data.List 
import Data.Ratio
import Data.Char
import Data.Fixed
import Data.Maybe
import Numeric (showFFloat)

import Syntax

-- realToComp turns a Double value into its corresponding Complex Double value
realToComp :: Double -> Complex Double
realToComp a = a :+ 0



--START: Calculations for quantum states as vectors--
-- Some useful constants:
h = 1/(sqrt 2)
hC = h :+ 0 -- h as a Complex Double value
c1 = 1.0 :+ 0 -- 1 as a Complex Double value
c0 = 0.0 :+ 0 -- 0 as a Complex Double value
i = 0.0 :+ 1 -- i as a Complex Double value
ang = \p -> 0.0 :+ p --angle for parameterized gates 
oneHalf = realToComp (1/2) -- 1/2 as a Complex Double value

--useful constants for vmag3
expk = \k -> exp(ang (pi/k))
constk = 1/(sqrt(rk + 1)) :+ 0.0
rk = realPart (expk 3)
ik = imagPart (expk 3)
value = (0:+(sqrt(2)*(realPart (expk 6))))

-- some unitary gates in matrix form:
had = fromLists [[hC,hC],[hC,-hC]] -- Hadamard gate
ident = fromLists [[c1,c0],[c0,c1]] -- Identity gate
x = fromLists [[c0,c1],[c1,c0]] -- Pauli X gate
y = fromLists [[c0,-i],[i,c0]] -- Pauli Y gate
z = fromLists [[c1,c0],[c0,-c1]] -- Pauli Z gate
sgt = fromLists[[c1, c0],[c0,i]] -- S gate
ph = \p -> fromLists[[c1, c0],[c0, exp(ang p)]] -- RZ parameterized gate
tof = fromLists [[1,0,0,0,0,0,0,0],
                 [0,1,0,0,0,0,0,0],
                 [0,0,1,0,0,0,0,0],
                 [0,0,0,1,0,0,0,0],
                 [0,0,0,0,1,0,0,0],
                 [0,0,0,0,0,1,0,0],
                 [0,0,0,0,0,0,0,1],
                 [0,0,0,0,0,0,1,0]] -- Toffoli
-- umag2 = fromLists[[hC, -(0.0:+h)],[-(0.0:+h), hC]]
-- vmag3 = fromLists[[constk*hC, 0 :+ 0, constk*(sqrt(rk) :+ 0), constk*(expk(3) / sqrt(2))],
--                   [constk*hC, 0 :+ 0, constk*(-(sqrt(rk) :+ 0.0)*expk(-3)), constk*(expk(-3) / sqrt(2))],
--                   [constk*(sqrt(rk) :+ 0), 0, constk*((expk(-6)*(ik:+0))/(0:+(sqrt(2)*(realPart (expk 6))))), constk*((-sqrt(rk)) :+ 0)],
--                   [0, constk*(sqrt(rk+1) :+ 0), 0, 0]]

-- vmag300 = fromLists[[constk*hC, 0 :+ 0],[constk*hC, 0 :+ 0]]
-- vmag301 = fromLists[[constk*(sqrt(rk) :+ 0), constk*(expk(3) / sqrt(2))],[constk*(-(sqrt(rk) :+ 0.0)*expk(-3)), constk*(expk(-3) / sqrt(2))]]
-- vmag310 = fromLists[[constk*(sqrt(rk) :+ 0), 0],[0, constk*(sqrt(rk+1) :+ 0)]]
-- vmag311 = fromLists[[constk*((expk(-6)*(ik:+0))/(0:+(sqrt(2)*(realPart (expk 6))))), constk*((-sqrt(rk)) :+ 0)],[0, 0]]
  
m0 = fromLists [[c1,0],[0,0]] -- measurement operator M0 = |0><0| 
m1 = fromLists [[0,0],[0,c1]] -- measurement operator M1 :+ 0 = |1 :+ 0><1 :+ 0|
m01 = fromLists [[0,c1],[0,0]]
m10 = fromLists [[0,0],[c1,0]]
m2 = fromLists [[0,0],[0,2*c1]] -- 2|1 :+ 0><1 :+ 0|
