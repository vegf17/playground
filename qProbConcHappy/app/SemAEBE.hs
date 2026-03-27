module SemAEBE where

import Syntax

-- changeSt i n s is the state [s|i=n] (here, we are not allowing attributions to non-declared
-- identifiers - see section 11.2 from the article) 
changeSt :: CVar -> DoubVal -> StC -> StC
changeSt i n [] = error ("changeSt (SemAEBE): variable: " ++ i ++ " is not declared")
changeSt i n ((i',n'):t) = if (i'==i)
                           then (i',n):(changeAll i n t)
                           else (i',n'):(changeSt i n t)

-- Auxiliary function to changeSt (changeAll i n s changes the value of all occurrences of i in s)
changeAll :: CVar -> DoubVal -> StC -> StC 
changeAll i n [] = []
changeAll i n ((i',n'):t)
    | (i==i') = (i',n):(changeAll i n t)
    | otherwise = (i',n'):(changeAll i n t)

--START: evaluate arithmetic expressions--
-- bigStepExp e s = n means that <e,s> ->* n (see Fig 3 from the article)
bigStepExp :: AExp -> StC -> DoubVal
bigStepExp (Num n) s = n -- 1st rule
bigStepExp (Var i) s = getDoubVal s i -- 3rd rule
bigStepExp Pi s = pi
bigStepExp (Sqrt e) s = let n = bigStepExp e s
                        in sqrt(n)
bigStepExp (Negate e) s = let n = bigStepExp e s
                        in -n
bigStepExp (Plus e1 e2) s = (bigStepExp e1 s) + (bigStepExp e2 s) -- final rules
bigStepExp (Minus e1 e2) s = (bigStepExp e1 s) - (bigStepExp e2 s) -- final rules
bigStepExp (Mult e1 e2) s = (bigStepExp e1 s) * (bigStepExp e2 s) -- final rules
bigStepExp (Div e1 e2) s = (bigStepExp e1 s) / (bigStepExp e2 s) -- final rules
--bigStepExp (Mod n exp) s = (bigStepExp exp s) `mod` n

-- getDoubVal s i = the value that state s attributes to identifier i
getDoubVal :: StC -> CVar -> DoubVal
getDoubVal [] i = error ("getDoubVal (SemAEBE): No value attributed to identifier \""++i++"\" was found.")
getDoubVal ((i',n'):t) i = if (i==i') then n' else getDoubVal t i
--END: evaluate arithmetic expressions--

--START: evaluate Boolean expressions--
-- bigStepBExp b s is the final result obtained by evaluating b in state s
bigStepBExp :: BExp -> StC -> Bool 
bigStepBExp (BTrue) s = True
bigStepBExp (BFalse) s = False
bigStepBExp (Not b) s = not (bigStepBExp b s)
bigStepBExp (And b1 b2) s = (bigStepBExp b1 s) && (bigStepBExp b2 s)
bigStepBExp (OrB b1 b2) s = (bigStepBExp b1 s) || (bigStepBExp b2 s)
bigStepBExp (Equ e1 e2) s = (bigStepExp e1 s) == (bigStepExp e2 s)
bigStepBExp (Leq e1 e2) s = (bigStepExp e1 s) <= (bigStepExp e2 s)
bigStepBExp (Geq e1 e2) s = (bigStepExp e1 s) >= (bigStepExp e2 s)
bigStepBExp (Less e1 e2) s = (bigStepExp e1 s) < (bigStepExp e2 s)
bigStepBExp (Gre e1 e2) s = (bigStepExp e1 s) > (bigStepExp e2 s)
--END: evaluate Boolean expressions--

--START: calculate free variables--
-- (freeE e) is the set of free identifiers in e
freeE :: AExp -> [CVar] 
freeE (Num n) = []
freeE (Var i) = [i]
freeE (Pi) = []
freeE (Plus e1 e2) = (freeE e1) ++ (freeE e2)
freeE (Minus e1 e2) = (freeE e1) ++ (freeE e2)
freeE (Mult e1 e2) = (freeE e1) ++ (freeE e2)
freeE (Div e1 e2) = (freeE e1) ++ (freeE e2)
freeE (Negate e) = freeE e
freeE (Sqrt e) = freeE e
--freeE (ModE n exp) = freeE exp

-- (freeB b) is the set of free identifiers in b
freeB :: BExp -> [CVar] 
freeB BTrue = []
freeB BFalse = []
freeB (Not b) = freeB b
freeB (And b1 b2) = (freeB b1) ++ (freeB b2)
freeB (OrB b1 b2) = (freeB b1) ++ (freeB b2)
freeB (Equ e1 e2) = (freeE e1) ++ (freeE e2)
freeB (Leq e1 e2) = (freeE e1) ++ (freeE e2)
freeB (Geq e1 e2) = (freeE e1) ++ (freeE e2)
freeB (Less e1 e2) = (freeE e1) ++ (freeE e2)
freeB (Gre e1 e2) = (freeE e1) ++ (freeE e2)

-- (belong ids s) is only True if no identifier in list ids is missing in state s
belong :: [CVar] -> StC -> Bool 
belong [] s = True
belong (i:t) s = if (declared i s) then belong t s else False 

-- (declared i s) is True if identifier i is part of state s
declared :: String -> StC -> Bool 
declared i [] = False
declared i ((i',n'):t) = if (i' == i) then True else (declared i t)
--END: calculate free variables--


--START: evaluate Arg expressions--
evalArg :: AExp -> DoubVal
evalArg (Num n) = n
evalArg Pi = pi
evalArg (Sqrt exp) = let n = evalArg exp
                     in sqrt(n)
evalArg (Plus exp1 exp2) = (evalArg exp1) + (evalArg exp2)
evalArg (Minus exp1 exp2) = (evalArg exp1) - (evalArg exp2)
evalArg (Mult exp1 exp2) = (evalArg exp1) * (evalArg exp2)
evalArg (Div exp1 exp2) = (evalArg exp1) / (evalArg exp2)
evalArg (Var s) = error "evalArg (SemAEBE): The evaluation of arguments should not use variables"
--START: evaluate Arg expressions--
