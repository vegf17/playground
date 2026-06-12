module Beautify_Vec where

import Data.Complex
import Data.Matrix
import Data.Char

import Syntax
--import Examples


--variable that defines the precision for doubles in states and probabilities, when showing the
--results
precision = 5


--START: Functions to transform quantum states, in the form of density operators, into the BraKet notation--

-- Convert a memory (sc,sq), which is composed by a classical state sc and a quantum state sq, to a string
memToString :: Mem -> String
memToString (sc,sq) =
  case null (rmvPlus $ vecToKetComplex sq) of
    True -> '[' : showStC sc ++ "]"
    False -> '[' : showStC sc ++ "], " ++ (rmvPlus $ vecToKetComplex sq)

--Convert a classical state to a string
--Example: showStC [("x1", n1), ("x2", n2), ..., ("xk", nk)] = "[(x1, n1), (x2, n2), ..., (xk, nk)]"
showStC :: StC -> String
showStC [] = ""
showStC ((st,int):[]) = "(" ++ st ++ "," ++ (show int) ++ ")"
showStC ((st,int):t) = "(" ++ st ++ "," ++ (show int) ++ ")," ++ showStC t

--Convert a quantum state, which is a density operator, to a string with complex numbers
--A quantum state is represented as a matrix, which can also be represented as \sum_i p_i |ψ_i><ψ_i| thus, 
--denOpToKetBraComplex \sum_i p_i |ψ_i><ψ_i| = "\sum_i p_i |ψ_i><ψ_i|"
--Example: denOpToKetBraComplex 0.5( |0><0| + i|0><1| - i|1><0| + |1><1|) = "0.5( |0><0| + i|0><1| - i|1><0| + |1><1|)"
vecToKetComplex :: StQ -> String
vecToKetComplex s = let m = toList s
                    in toKet m (numQbits $ length m)

--Map a vector to its ket notation
--toKet v nqbits = string that represents the ket notation of a vector v with a number of qubits nqubits
--Example: toKet [c1, c2, c3, c4] 2 = "c1|00> + c2|01> + c3|10> + c4|11>"
--Example: toKet [c1, c2, c3, 0] 2 = "c1|00> + c2|01> + c3|10>"
toKet :: [Complex Double] -> Int -> String
toKet v nqbits = rmvPlus $ toKetAux v 0 nqbits

--toKetAux v nqbits = string that represents the ket notation of a vector v with a number of qubits nqubits
--Note that there is an extra plus sign at the end of the ket notation for some cases (this sign is
--removed in the toKet function through the rmvPlus function)
--Example: toKetAux [c1, c2, c3, c4] 0 2 = "c1|00> + c2|01> + c3|10> + c4|11>"
--Example: toKetAux [c1, c2, c3, 0] 0 2 = "c1|00> + c2|01> + c3|10> + "
toKetAux :: [Complex Double] -> Int -> Int -> String
toKetAux [] _ _ = ""
toKetAux (h:[]) n nqbits = if h == 0 :+ 0 then ""
                           else "(" ++ complexToString(h) ++ ")|" ++ (toBin nqbits n) ++ ">"
toKetAux (h:t) n nqbits = if h == 0 :+ 0 then toKetAux t (n+1) nqbits
                       else  "(" ++ complexToString(h) ++ ")|" ++ (toBin nqbits n) ++ "> + " ++ toKetAux t (n+1) nqbits
                                 
--Remove the last three characters of the input whenever '+' is the penultimate char
--Example: rmvPlus "<00| + <01| + <10| + " = "<00| + <01| + <10|"
rmvPlus :: String -> String
rmvPlus [] = ""
rmvPlus s = let n = length s
            in if s!!(n-2)=='+' then take (n-3) s
               else s
                    
--toBin nbits n = n as a binary with at least nbits
--Example: toBin 2 3 = "11"
--Example: toBin 2 1 = "01"
--Example: toBin 2 4 = "100"
toBin :: Int -> Int -> String
toBin nbits n = padZeros nbits (reverse $ toBinAux n)
  where
    toBinAux 0 = []
    toBinAux n = intToDigit (n `mod` 2) : toBinAux (n `div` 2)
    padZeros nqbits str = replicate (nbits - length str) '0' ++ str

--Calculate the number of qubits associated to a density operator representing a quantum state
--numQbits n =  returns the least integer not less than log_2(n)
--This function is useful to obtain the number of qubits associated to a density operator n x n 
--Example: numQbits 3 = 2
numQbits :: Int -> Int
numQbits n = ceiling $ logBase 2 (fromIntegral n)

--Remove elements whoose probability is zero or infinite
--Example: clean [(mem1, 0.5), (mem2, 0.0), (mem3, infty)] = [(mem1, 0.5)]
--clean :: [(Mem, DoubProb)] -> [(Mem, DoubProb)]
--clean l = filter cond l
--  where cond = (\(_,p) -> (p/=0) && (isInfinite p == False))

--END: Functions to transform quantum states, in the form of density operators, into the BraKet notation--


--START: Functions for showing results--
--The functions limitPrec, limitPrecDouble, limitPrecisionComplex, and limitPrecisionS are used to
--limit the number of digits that are used when displaying the coefficients and/or the probabilities
--of states
limitPrec :: Int -> [[(Mem, DoubProb)]] -> [[(Mem, DoubProb)]]
limitPrec _ [] = []
limitPrec n (h:t) = limitPrecAux n h : limitPrec n t

limitPrecAux :: Int -> [(Mem, DoubProb)] -> [(Mem, DoubProb)]
limitPrecAux n l = map (\((sc,sq),p) -> ((sc,limitPrecisionS n sq), limitPrecDouble n p)) l

limitPrecDouble :: Int -> Double -> Double
limitPrecDouble precision x = fromIntegral (round (x * 10^precision)) / 10^precision

limitPrecisionComplex :: Int -> Complex Double -> Complex Double
limitPrecisionComplex precision (r :+ i) =
  fromRealIntegral (roundFrac precision r) :+ fromRealIntegral (roundFrac precision i)
  where
    roundFrac :: Int -> Double -> Double
    roundFrac p x = fromIntegral (round (x * 10^p)) / 10^p
    fromRealIntegral :: Real a => a -> Double
    fromRealIntegral = realToFrac

limitPrecisionS :: Int -> StQ -> StQ
limitPrecisionS n st = fromLists [ f l | l <- lst]
  where lst = toLists st
        f = map (\e -> limitPrecisionComplex n e)


limPrecKStep :: Int -> [([(Mem, DoubProb)], Double)] -> [([(Mem, DoubProb)], Double)]
limPrecKStep n l = [(limitPrecAux n dist, limitPrecDouble n q) | (dist, q) <- l]

  
-- (showProbMemList l) = String value corresponding to the (Mem, DoubProb) values inside l, with a comma and a
-- new line character separating them
showProbMemList :: [(Mem, DoubProb)] -> String 
showProbMemList [] = ""
showProbMemList [c] = showProbMem c
showProbMemList (h:t) = (showProbMem h) ++ " +\n" ++ (showProbMemList t)

-- showProbMem (mem,p) = String value corresponding to (mem,p)
showProbMem :: (Mem, DoubProb) -> String 
showProbMem ((sc,sq),p) = let opDen = rmvPlus $ vecToKetComplex sq
                          in case null opDen of
                               True -> (show p) ++ "·([" ++ showStC sc ++ "])"
                               False -> (show p) ++ "·([" ++ showStC sc ++ "], " ++ (rmvPlus $ vecToKetComplex sq) ++ ")"

-- showRun (s, md) = String value corresponding to the name of the program being executed, s,
-- together with its results, md
showRun :: (String, [(Mem, DoubProb)]) -> String
--showRun (s,md) = s ++ ": \n" ++ (showProbMemList $ addDistBeaut md) ++ "\n"
showRun (s,md) = s ++ ": \n" ++ (showProbMemList $ limitPrecAux 5 (addDistBeaut md)) ++ "\n"
--showRun (s,md) = s ++ ": \n" ++ showProbMemList (limitPrecAux 5 md) ++ "\n"

addDistBeaut :: [(Mem, DoubProb)] -> [(Mem, DoubProb)]
addDistBeaut [] = []
addDistBeaut dist@((mem,prob):t) = (mem, sum [p | (m,p) <- dist,  m==mem]) : addDistBeaut [(m,p) | (m,p) <- t, m/=mem]

-- addDistBeaut :: (Ord a) => [(a, Double)] -> [(a, Double)]
-- addDistBeaut [] = []
-- addDistBeaut dist@((mem,prob):t) = (mem, sum [p | (m,p) <- dist,  m==mem]) : addDistBeaut [(m,p) | (m,p) <- t, m/=mem]

--Convert a complex number to a string
--If the complex number is only composed of the real part, it will only display the real part;
--similarly whenever the complex number is only composed of the imaginary part
complexToString :: Complex Double -> String 
complexToString (r:+i)
  | r==0 && i==0 = ""
  | r==0 && i/=0 = show(i) ++ "i"
  | r/=0 && i==0 = show(r)
  | i<0 = show(r) ++ show(i)++ "i"
  | otherwise = show(r) ++ "+" ++ show(i) ++ "i"
--END: Functions for showing results--


--START: Functions for showing results when IO and convex coefficients are explicit--
-- showRunK (s, md) = String value corresponding to the name of the program being executed, s,
-- together with its results, md
showRunK :: (String, [([(Mem, DoubProb)], Double)]) -> String
showRunK (s,md) = "\n" ++ s ++ ": \n" ++ showProbProbMemList md ++ "\n"

-- showProbProbMemList l = String value corresponding to the ([(Mem, DoubProb)],Double) values inside
-- l, with a new line character separating them
showProbProbMemList :: [([(Mem, DoubProb)], Double)] -> String
showProbProbMemList [] = ""
showProbProbMemList [h] = showProbProbMem h
showProbProbMemList (h:t) = (showProbProbMem h) ++ "\n" ++ (showProbProbMemList t)

-- showProbProbMem (dist,p) = String value corresponding to (dist,p)
showProbProbMem :: ([(Mem, DoubProb)], Double) -> String
showProbProbMem (dist, q) = (show q) ++ " -> " ++  (showProbMemListK dist)

-- (showProbMemListK l) is similar to (showProbMemList l)
showProbMemListK :: [(Mem, DoubProb)] -> String 
showProbMemListK [] = ""
showProbMemListK [c] = showProbMem c
showProbMemListK (h:t) = (showProbMem h) ++ " +\n\t" ++ (showProbMemListK t)
--END: Functions for showing results when IO and convex coefficients are explicit--
