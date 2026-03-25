module Examples where

import Data.Matrix
import Data.Complex
import Data.List 
import Data.Ratio
import Data.Char
import Data.Fixed
import Numeric (showFFloat)

import QuantumCalc
import Syntax
import Gates

--classical state--
s1 = [("a",0), ("b",0), ("c",0)]
s2 = [("a",0)]
s3 = [("a",0), ("b",0)]
s4 = [("a",0),("i",0)]
--classical state--

--linking function--
l("q1") = 1
l("q2") = 2
l("q3") = 3

l1 = [("q1",1),("q2",2),("q3",3)] :: [(String,Int)]
l2 = [("q1",1)] :: [(String,Int)]
l4 = [("q1",1),("q2",2)] :: [(String,Int)]
lmem1 = (s1,l1,stH0) -- <-- estados
lmem2 = (s2,l2,rho0) -- <-- operador densidade
lmem3 = (s1,l2,fromLists [[]]) -- <-- operador densidade
lmem4 = (s2, l4, rho00)
lmem5 = (s3,[],fromLists [[]])
lmem6 = (s3, l1, rho000) 
--linking function--



--mem--
mem1 = [((s2,rho0),0.5),((s2,rho1),0.4)]
--mem--

--quantum states--
st0new = fromLists [[4.0 :+ 0.0],[c0]] --  |0>
st0 = fromLists [[c1],[c0]] --  |0>
st1 = fromLists [[c0],[c1]] --  |1>
stH0 = fromLists [[hC],[hC]] --  H|0>
stH1 = fromLists [[hC],[-hC]] --  H|1>
st00 = fromLists [[c1],[c0],[c0],[c0]] --  |00>
st01 = fromLists [[c0],[c1],[c0],[c0]] --  |01>
st10 = fromLists [[c0],[c0],[c1],[c0]] --  |10>
st11 = fromLists [[c0],[c0],[c0],[c1]] --  |11>
phiPlus = fromLists $ map (map realToComp) [[h],[0],[0],[h]] -- 1/sqrt(2) (|00> + |11>)
phiMinu = fromLists $ map (map realToComp) [[h],[0],[0],[-h]] -- 1/sqrt(2) (|00> - |11>)
psiPlus = fromLists $ map (map realToComp) [[0],[h],[h],[0]] -- 1/sqrt(2) (|01> + |10>)
psiMinu = fromLists $ map (map realToComp) [[0],[h],[-h],[0]] -- 1/sqrt(2) (|01> - |10>)
st2Q = fromLists [[oneHalf],[oneHalf],[oneHalf],[oneHalf]] --  1/2 (|00> + |01> + |10> + |11>)
stH00 = multElem h (st00 + st10) -- 1/sqrt(2) (|00> + |10>)
st000 = tensorProduct [st0,st0,st0] --  |000>
st001 = tensorProduct [st0,st0,st1] --  |001>
st010 = tensorProduct [st0,st1,st0] --  |010>
st011 = tensorProduct [st0,st1,st1] --  |011>
st100 = tensorProduct [st1,st0,st0] --  |100>
st101 = tensorProduct [st1,st0,st1] --  |101>
st110 = tensorProduct [st1,st1,st0] --  |110>
st111 = tensorProduct [st1,st1,st1] --  |111>
ghz = multElem h (st000 + st111) -- 1/sqrt(2) (|000> + |111>)
--quantum states--

--density operators--
rho0 = tensorProduct [st0, dagger st0] --  |0><0|
rho1 = tensorProduct [st1, dagger st1] --  |1><1|
rhoH0 = tensorProduct [stH0, dagger stH0] --  H|0><0|H^+
rhoH1 = tensorProduct [stH1, dagger stH1] --  H|1><1|H^+
rho00 = tensorProduct [st00, dagger st00] --  |00><00|
rho0001 = tensorProduct [tensorProduct [st0, st0], dagger $ tensorProduct [st0, st1]] --  |00><01|
rho0010 = tensorProduct [tensorProduct [st0, st0], dagger $ tensorProduct [st1, st0]] --  |00><10|
rho0011 = tensorProduct [tensorProduct [st0, st0], dagger $ tensorProduct [st1, st1]] --  |00><11|
rho0100 = tensorProduct [tensorProduct [st0, st1], dagger $ tensorProduct [st0, st0]] --  |01><00|
rho0101 = tensorProduct [tensorProduct [st0new, st1], dagger $ tensorProduct [st0, st1]] --  |01><01|
rho0110 = tensorProduct [tensorProduct [st0, st1], dagger $ tensorProduct [st1, st0]] --  |01><10|
rho0111 = tensorProduct [tensorProduct [st0, st1], dagger $ tensorProduct [st1, st1]] --  |01><11|
rho01 = tensorProduct [st01, dagger st01] --  |01><01|
rho10 = tensorProduct [st10, dagger st10] --  |10><10|
rho11 = tensorProduct [st11, dagger st11] --  |11><11|
rho000 = tensorProduct [st000, dagger st000] --  |000><000|

rhoPhiPlus = tensorProduct [phiPlus, dagger phiPlus]
rho2Q = tensorProduct [st2Q, dagger st2Q]
rho = tensorProduct [tensorProduct [phiPlus, st1], dagger $ tensorProduct [phiPlus, st1]]
--density operators--

--examples
sk = Skip --sk
asga = Asg "a" (Num 1) -- a:=1
asgb = Asg "b" (Num 1) -- b:=1
asgc = Asg "c" (Num 2) -- c:=2
seq1 = Seq sk asga -- sk;a:=1
seq2 = Seq asga asgb -- a:=1;b:=1
seq3 = Seq seq2 asgc --a:=1;b:=1;c:=2
seq4 = Seq (Or sk (P 0.5 sk asga)) sk -- (sk or (sk +_{0.5} a:=1));sk
seq5 = Seq (Or (P 0.5 asga asgb) (P 0.5 sk asga)) sk -- ((a:=1 +_{0.5} b:=1) or (sk +_{0.5} a:=1));sk
seq6 = Seq (P 0.5 (Seq sk asga) asgb) sk -- ((sk;a:=1 +_{0.5} b:=1));sk
prob1 = P 0.5 sk sk -- sk +_{0.5} sk
prob2 = P 0.3 sk asga -- sk +_{0.3} a:=1
prob3 = P 0.4 seq2 asgc -- a:=1;b:=1 +_{0.4} c:=2
prob4 = P 0.4 seq1 seq2 -- sk;a:=1 +_{0.4} a:=1;b:=1
prob5 = P 0.4 or4 or3 -- (sk;a:=1 or a:=1;b:=1) +_{0.4} (sk;a:=1 or sk)
prob6 = P 0.5 asga asgb -- a:=1 +_{0.5} b:=1
or1 = Or sk sk -- sk or sk 
or2 = Or sk asga -- sk or a:=1
or3 = Or seq1 sk -- sk;a:=1 or sk 
or4 = Or seq1 seq2 -- sk;a:=1 or a:=1;b:=1
or5 = Or prob4 prob3 -- (sk;a:=1 +_{0.4} a:=1;b:=1) or (sk;a:=1 +_{0.4} sk)
or6 = Or seq2 sk -- a:=1;b:=1 or sk
or7 = Or prob6 asgc -- (a:=1 +_{0.5} b:=1) or c:=2
or8 = Or prob3 asgc -- (a:=1;b:=1 +_{0.4} c:=2) or c:=2
or9 = Or (Asg "a" (Num 2)) asga -- a:=2 or a:=1
or10 = Or (P 0.5 (Seq asga sk) (Asg "a" (Num 2))) (Seq (Asg "a" (Num 3)) sk) -- (a:=1;sk +_{0.5} a:=2) or (a:=3;sk)
or11 = Or (Or asga (Asg "a" (Num 2))) ((Asg "a" (Num 3))) -- (a:=1 or a:=2) or a:=3
or12 = Or asga (Asg "a" (Num 2))
par1 = Par asga asgb -- a:=1 || b:=1
par2 = Par seq1 asgb -- sk;a:=1 || b:=1
par3 = Par or2 prob5 -- (sk or a:=1) || ((sk;a:=1 or a:=1;b:=1) +_{0.4} (sk;a:=1 or sk)) PASSOU TESTE
par4 = Par or2 (P 0.4 (Or sk asga) (Or sk sk)) -- (sk or a:=1) || ((sk or a:=1) +_{0.4} (sk or sk)) 
par5 = Par or2 (P 0.4 (Or (Seq sk asga) asga) (Or sk sk)) -- (sk or a:=1) || ((sk;a:=1 or a:=1) +_{0.4} (sk or sk)) PASSOU TESTE
par6 = Par asga (P 0.4 (Seq sk asga) asgb) -- (a:=1) || ((sk;a:=1) +_{0.4} b:=1)  PASSOU TESTE
par7 = Par asga (Or (Seq sk asga) (P 0.5 asgb sk)) -- (a:=1) || ((sk;a:=1) +_{0.4} (b:=1 or sk))  
while1 = Whl BTrue or9 -- while true -> a:=2 or a:=1
while2 = Whl BFalse or9 -- while false -> a:=2 or a:=1

idq1 = U I ["q1"] -- I(q1)
hq1 = U H ["q1"] -- H(q1)
hq2 = U H ["q2"] -- H(q2)
hq3 = U H ["q3"] -- H(q3)
xq1 = U X ["q1"] -- X(q1)
xq2 = U X ["q2"] -- X(q2)
xq3 = U X ["q3"] -- X(q3)
zq1 = U Z ["q1"] -- Z(q1)
zq2 = U Z ["q2"] -- Z(q2)
zq3 = U Z ["q3"] -- Z(q3)
cnotq1q2 = U CNOT ["q1","q2"] -- CNOT(q1,q2)
cnotq2q1 = U CNOT ["q2","q1"] -- CNOT(q2,q1)
czq1q2 = U CZ ["q1","q2"] -- CZ(q1,q2)
czq2q1 = U CZ ["q2","q1"] -- CZ(q2,q1)
measq1 = Meas ("a","q1") -- M(a <- q1)
measq2 = Meas ("b","q2") -- M(b <- q2)
qseq1 = Seq hq1 measq1 -- H(q1); M(a <- q1)
qseq2 = Seq qseq1 qif1 -- H(q1); M(a <- q1); if -> (a<=0, a:=1,X(q1))
qseq3 = Seq hq1 xq1 -- H(q1);X(q1)
qseq3a = Seq hq2 xq2 -- H(q2);X(q2)
qseq4 = Seq qseq3 qseq3 -- H(q1);X(q1);H(q1);X(q1)
qseq5 = Seq hq1 cnotq1q2 -- H(q1);CNOT(q1,q2)
qor1 = Or measq1 hq1 -- M(a <- q1) or H(q1)
qor2 = Or (qseq3) hq1 -- H(q1);X(q1) or H(q1)
qpar1 = Par hq1 measq1 -- H(q1) || M(a <- q1)
qpar2 = Par measq1 xq1 -- M(a <- q1) || X(q1) 
qif1 = IfC (Leq (Var "a") (Num 0)) asga xq1 -- if -> (a<=0, a:=1,X(q1))
qwhile1 = Whl (Leq (Var "a") (Num 0)) qseq1 -- while (a<=0) -> H(q1); M(a <- q1)
qwhile2 = Whl (Leq (Var "a") (Num 0)) qpar1 -- while (a<=0) -> H(q1) || M(a <- q1)
tof123 = U TOF ["q1","q2","q3"] -- TOF(q1,q2,q3)
--error1 = Seq qseq3 (Seq qseq3 cnotq1q2)
error1 = Seq hq1 cnotq1q2
--error2 = Seq qseq3a (Seq qseq3a hq2)
error2 = Seq qseq3a hq2
errore = Seq error1 (Seq error2 (Seq measq1 measq2))

man = Par (Par hq1 (U Y ["q1"])) (xq1)


--Quantum Teleport--
stc = [("x1",0),("x2",0)]
stq = \s -> tensorProduct[tensorProduct [s, dagger s], rho00]
l3 = [("q1",1),("q2",2),("q3",3)] :: [(String,Int)]
mem = \s -> (stc,l3,stq s)

--sequential version
charlie = Seq (U H ["q2"]) (U CNOT ["q2","q3"])
aliP = Seq cnotq1q2 hq1
aliM = Seq (Meas("x2","q2")) (Meas("x1","q1"))
alice = Seq aliP aliM
bob = Seq (IfC (Equ (Var "x2") (Num 0)) sk (U X ["q3"])) (IfC (Equ (Var "x1") (Num 0)) sk (U Z ["q3"]))
qTele = Seq charlie (Seq alice bob)

--await version
-- charlieA = SeqA (UA H ["q2"]) (UA CNOT ["q2","q3"])
-- aliPA = SeqA (UA CNOT ["q1","q2"]) (UA H ["q1"])
-- aliMA = SeqA (MeasA("x1","q1")) (MeasA("x2","q2"))
-- aliceA = SeqA aliPA aliMA
-- bobA = SeqA (IfCA (Equ (Var "x2") (Num 0)) SkipA (UA X ["q3"])) (IfCA (Equ (Var "x1") (Num 0)) SkipA (UA Z ["q3"]))
-- actCharlie = Asg "xC" (Num 1)
-- awaitCharlie = Await (Equ (Var "xC") (Num 1)) (
--   SeqA charlieA (SeqA (AsgA "xC" (Num 0)) (AsgA "xA" (Num 1)))
--   )
-- awaitAlice = Await (Equ (Var "xA") (Num 1)) (
--   SeqA aliceA (SeqA (AsgA "xA" (Num 0)) (AsgA "xB" (Num 1)))
--   )
-- awaitBob = Await (Equ (Var "xB") (Num 1)) (SeqA bobA (AsgA "xB" (Num 0)))
-- qtTeleAwait = Par actCharlie (Par awaitCharlie (Par awaitAlice awaitBob))
-- stcAwait = [("x1",0),("x2",0),("xC",0),("xB",0),("xA",0)]
-- memAwait = \s -> (stcAwait,l3,stq s)

charlieA = Seq (U H ["q2"]) (U CNOT ["q2","q3"])
aliPA = Seq (U CNOT ["q1","q2"]) (U H ["q1"])
aliMA = Seq (Meas("x1","q1")) (Meas("x2","q2"))
aliceA = Seq aliPA aliMA
bobA = Seq (IfC (Equ (Var "x2") (Num 0)) Skip (U X ["q3"])) (IfC (Equ (Var "x1") (Num 0)) Skip (U Z ["q3"]))
actCharlie = Asg "xC" (Num 1)
awaitCharlie = Await (Equ (Var "xC") (Num 1)) (
  Seq charlieA (Seq (Asg "xC" (Num 0)) (Asg "xA" (Num 1)))
  )
awaitAlice = Await (Equ (Var "xA") (Num 1)) (
  Seq aliceA (Seq (Asg "xA" (Num 0)) (Asg "xB" (Num 1)))
  )
awaitBob = Await (Equ (Var "xB") (Num 1)) (Seq bobA (Asg "xB" (Num 0)))
qtTeleAwait = Par awaitCharlie (Par actCharlie (Par awaitAlice awaitBob))
stcAwait = [("x1",0),("x2",0),("xC",0),("xB",0),("xA",0)]
memAwait = \s -> (stcAwait,l3,stq s)


--drunk Bob
pBob = Par (IfC (Leq (Var "x2") (Num 0)) sk (U X ["q3"])) (IfC (Leq (Var "x1") (Num 0)) sk (U Z ["q3"]))
qTeleP = Seq (Seq aliP aliM) pBob

--Grover (2 qbits)
--Sequential
--  |q1q2q3>
gro_in = Seq (U H ["q1"]) (U H ["q2"])
gro_anc = Seq (U X ["q3"]) (U H ["q3"])
oracle00 = Seq (Seq xq1 xq2) (Seq tof123 (Seq xq1 xq2))
oracle01 = Seq (Seq xq1 tof123) xq1
oracle10 = Seq (Seq xq2 tof123) xq2
oracle11 = tof123
gro_diff = Seq (Seq hq1 xq1) (Seq hq2 (Seq xq2 (Seq czq1q2 (Seq xq1 (Seq hq1 (Seq xq2 hq2))))))
grover00 = Seq gro_in (Seq gro_anc (Seq oracle00 gro_diff))
grover01 = Seq gro_in (Seq gro_anc (Seq oracle01 gro_diff))
grover10 = Seq gro_in (Seq gro_anc (Seq oracle10 gro_diff))
grover11 = Seq gro_in (Seq gro_anc (Seq oracle11 gro_diff))
grover = Seq (Seq gro_in gro_anc) (Seq (Or (Or oracle00 oracle01) (Or oracle10 oracle11)) gro_diff)
grover_meas = Seq grover (Seq (Meas ("x1", "q1")) (Meas ("x2", "q2")))
csGrover = [("x1",0), ("x2",0)]
lmemGrover = (csGrover, l3, rho000)


--debug nstepQCSch
com = Whl BTrue (Or sk xq1)
probpath = ([],(com,lmem2))
--[((com,lmem2),[Right ((Seq sk com,lmem2),1.0)]),((Seq sk com,lmem2),[Right ((com,lmem2),1.0)])]

--Example that shows that we do not have: if \Downarrow{S,k} then -->>
refuse = Par (Whl (Leq (Var "a") (Num 0)) sk) asga -- while(a<= 0, sk) || a:=1


--Prob coin toss
--iterations to obtain a probability: k=2+3*n for n>=0 where n is the stoppage number
lmem7 = (s4,l2,rho0)
asgi = Asg "i" (Num 1)
probTossCoin = \num -> (Whl (Less (Var "i") (Num num))
  (Seq (P 0.5 asga (Asg "a" (Num 0)))
       (Asg "i" (Plus (Var "i") (Num (1)))))) 


--Prob random walk
--iterations to obtain a probability (only one): k=2+3*n for n>=0 where n is the stoppage number
scRandWalk = [("i1",0),("x1",0),("i2",0),("x2",0),("i3",0),("x3",0),("i4",0),("x4",0)]
lmemRandWalk = (scRandWalk, [], fromLists [[]])
plusOne = \var -> Asg var (Plus (Var var) (Num (1)))
minusOne = \var -> Asg var (Plus (Var var) (Num (-1)))
probRandWalk = \(it, num, var) -> (Whl (Less (Var it) (Num num))
  (Seq (P 0.5 (plusOne var) (minusOne var))
       (Asg it (Plus (Var it) (Num (1))))))                                  
probRandWalkConc = \num -> Par (Par (probRandWalk ("i1",num,"x1")) (probRandWalk ("i2",num,"x2")))
                                 (Par (probRandWalk ("i3",num,"x3")) (probRandWalk ("i4",num,"x4")))
                       

--Quantum coin toss
--iterations to obtain a probability: 2+4*n for n>=0 where n is the stoppage number
lQuantTossCoin = [("q1", 1)] :: [(String, Int)]
lmemQuantTossCoin = ([("i", 0)], lQuantTossCoin, rho0) 
quantTossCoin = \num -> (Whl (Less (Var "i") (Num num))
  (Seq (Seq hq1 measq1)
       (Asg "i" (Plus (Var "i") (Num (1)))))) 

--Quantum Entanglement
lmemEnt = ([("x",0)], l4, rho00)
qtEnt = Seq hq1 (Seq cnotq1q2 (Meas ("x","q1")))


--Test commands from CAwait
-- ifA = IfCA (Leq (Var "a") (Num 0)) (UA X ["q1"]) (SkipA)
-- seq1A = SeqA (UA H ["q1"]) (SeqA (MeasA("a","q1")) ifA)
-- awaitA = Await (BTrue) seq1A


--Test Atom from Await (lmem2)
await1 = Await BTrue Skip
await2 = Await BFalse Skip
await3 = Await (Equ (Var "a") (Num (0))) hq1
atom1 = Atom prob3 --Atom(a:=1;b:=1 +_{0.4} c:=2)    --lmem3
atom2 = Atom qwhile2 --Atom(while (a<=0) -> H(q1) || M(a <- q1))
atom3 = Seq (Atom (Seq (Asg "a" (Num 1)) (Asg "a" (Num 2)))) (Skip) -- Atom(a:=1;a:=2);skip
atom4 = Or sk atom3 -- sk or Atom(a:=1;a:=2);skip
atom5 = Par asga atom3 --  (a:=1) || (Atom(a:=1;a:=2);skip)
atom6 = Par (Par atom1 atom2) atom3 -- (Atom(a:=1;b:=1 +_{0.4} c:=2)) || (Atom(while (a<=0) -> H(q1) || M(a <- q1))) || (Atom(a:=1;a:=2);skip)
atom7 = Par (Par (Atom(Seq (Asg "a" (Num 1)) (Asg "a" (Num 2)))) (Atom(Seq (Asg "a" (Num 3)) (Asg "a" (Num 4))))) (Asg "a" (Num 5)) -- (Atom(a:=1;a:=2)) || (Atom(a:=3 or a:=4)) || a:=5
atom8 =  P (1%2) (Par (Atom (Seq asga (Asg "a" (Num 2)))) ((Asg "a" (Num 3)))) (Asg "a" (Num 5)) -- (Atom(a:=1; a:=2) || a:=3) +_{0.5} a:=5
atom9 = Par (Atom asga) (Atom asgb)
