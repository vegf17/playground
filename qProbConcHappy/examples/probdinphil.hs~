dinPhil :: Int -> Int -> String
dinPhil nphil n = if (nphil>n)
                  then ""
                  else if (nphil==n)
                       then dinPhilAux nphil n
                       else case nphil of
                         1 -> dinPhilAux1 nphil n ++ "\n ||\n"  ++ (dinPhil (nphil+1) n)
                         otherwise -> (dinPhilAux nphil n) ++ "\n ||\n" ++ (dinPhil (nphil+1) n)

dinPhilAux :: Int -> Int-> String
dinPhilAux nphil n =
 " while(true) do{\n  while(st" ++ (show nphil)
 ++"==1) do{st" ++ (show nphil)
 ++":=2 or skip};\n  (try" ++ (show nphil)
 ++":=1 or try" ++ (show nphil)
 ++":=0);\n  while(try" ++ (show nphil)
 ++"==1) do{\n   chops" ++ (show nphil)
 ++":=-1 (+)(1%2) chops" ++ (show nphil)
 ++":=1;\n   if(chops" ++ (show nphil)
 ++"==1)\n    then {\n     await(cs" ++ (show nphil)
 ++"==1) do {cs" ++ (show nphil)
 ++":=cs" ++ (show nphil) ++ "-1};\n     if(cs" ++ (show (nphil-1))
 ++"==1) then {cs" ++ (show (nphil-1))
 ++":=cs" ++ (show (nphil-1)) ++ "-1; try" ++ (show nphil)
 ++":=0}\n     else {cs" ++ (show nphil)
 ++":=cs" ++ (show nphil) ++ "+1}\n    }\n    else{\n     await(cs" ++ (show (nphil-1))
 ++"==1) do {cs" ++ (show (nphil-1))
 ++":=cs" ++ (show (nphil-1)) ++ "-1};\n     if(cs" ++ (show nphil)
 ++"==1) then {cs" ++ (show nphil)
 ++":=cs" ++ (show nphil) ++ "-1; try" ++ (show nphil)
 ++":=0}\n     else {cs" ++ (show (nphil-1))
 ++":=cs" ++ (show (nphil-1)) ++ "+1}\n    }\n  };\n  st" ++ (show nphil)
 ++":=0;\n  while(st" ++ (show nphil)
 ++"==0) do {st" ++ (show nphil)
 ++":=1 or skip};\n  eat" ++ (show nphil)
 ++":=eat" ++ (show nphil)
 ++"+1; cs" ++ (show nphil)
 ++":=cs" ++ (show nphil) ++"+1; cs" ++ (show (nphil-1))
 ++":=cs" ++ (show (nphil-1)) ++ "+1\n }"


dinPhilAux1 :: Int -> Int-> String
dinPhilAux1 nphil n =
 " while(true) do{\n  while(st" ++ (show nphil)
 ++"==1) do{st" ++ (show nphil)
 ++":=2 or skip};\n  (try" ++ (show nphil)
 ++":=1 or try" ++ (show nphil)
 ++":=0);\n  while(try" ++ (show nphil)
 ++"==1) do{\n   chops" ++ (show nphil)
 ++":=-1 (+)(1%2) chops" ++ (show nphil)
 ++":=1;\n   if(chops" ++ (show nphil)
 ++"==1)\n    then {\n     await(cs" ++ (show nphil)
 ++"==1) do {cs" ++ (show nphil)
 ++":=cs" ++ (show nphil) ++ "-1};\n     if(cs" ++ (show n)
 ++"==1) then {cs" ++ (show n)
 ++":=cs" ++ (show n) ++ "-1; try" ++ (show nphil)
 ++":=0}\n     else {cs" ++ (show nphil)
 ++":=cs" ++ (show nphil) ++ "+1}\n    }\n    else{\n     await(cs" ++ (show n)
 ++"==1) do {cs" ++ (show n)
 ++":=cs" ++ (show n) ++ "-1};\n     if(cs" ++ (show nphil)
 ++"==1) then {cs" ++ (show nphil)
 ++":=cs" ++ (show nphil) ++ "-1; try" ++ (show nphil)
 ++":=0}\n     else {cs" ++ (show n)
 ++":=cs" ++ (show n) ++ "+1}\n    }\n  };\n  st" ++ (show nphil)
 ++":=0;\n  while(st" ++ (show nphil)
 ++"==0) do {st" ++ (show nphil)
 ++":=1 or skip};\n  eat" ++ (show nphil)
 ++":=eat" ++ (show nphil)
 ++"+1; cs" ++ (show nphil)
 ++":=cs" ++ (show nphil) ++"+1; cs" ++ (show n)
 ++":=cs" ++ (show n) ++ "+1\n }"

stDinPhil :: Int -> String
stDinPhil  n = let init1 = [ "(" ++ var ++ show id ++ "," ++ show 1 ++ ")" | var <- ["st", "cs"], id <- [1 .. n]]
                   init0 = [ "(" ++ var ++ show id ++ "," ++ show 0 ++ ")" | var <- ["try", "chops", "eat"], id <- [1 .. n]]
                   init = init1 ++ init0
               in "[" ++ stDinPhilAux init ++ "]"
                  
stDinPhilAux :: [String] -> String
stDinPhilAux [] = ""
stDinPhilAux [h] = h
stDinPhilAux (h:t) = h ++ "," ++ stDinPhilAux t


lazy :: Int -> Int -> Int -> String
lazy k nphil n =
  "---ProbDinPhil" ++ show n ++ "---\n"
  ++"hist: (1, [])\n"
  ++"k: " ++ show k
  ++"\n<\n"
  ++ (dinPhil nphil n)
  ++",\n "
  ++ (stDinPhil n)
  ++"\n>\n---ProbDinPhil" ++ show n ++ "---\n"  
