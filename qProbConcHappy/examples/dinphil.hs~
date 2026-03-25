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
  ++"==1) do{\n   st" ++ (show nphil)
  ++":=2 or skip\n  };\n  await(cs" ++ (show nphil)
  ++"==1 & cs" ++ (show (nphil-1))
  ++"==1) do{\n   cs" ++ (show nphil)
  ++ ":=cs" ++ (show nphil) ++ "-1;\n   cs" ++ (show (nphil-1))
  ++":=cs" ++ (show (nphil-1)) ++ "-1\n  };\n  st" ++ (show nphil)
  ++":=0;\n  while(st" ++ (show nphil)
  ++"==0) do{\n   st" ++ (show nphil)
  ++":=1 or skip  \n  };\n   eat" ++ (show nphil)
  ++":=eat" ++ (show nphil)
  ++"+1;\n   cs" ++ (show nphil)
  ++":=cs" ++ (show nphil) ++"+1;\n   cs" ++ (show (nphil-1))
  ++":=cs" ++ (show (nphil-1)) ++ "+1\n }"


dinPhilAux1 :: Int -> Int-> String
dinPhilAux1 nphil n =
  " while(true) do{\n  while(st" ++ (show nphil)
  ++"==1) do{\n   st" ++ (show nphil)
  ++":=2 or skip\n  };\n  await(cs" ++ (show nphil)
  ++"==1 & cs" ++ (show n)
  ++"==1) do{\n   cs" ++ (show nphil)
  ++ ":=cs" ++ (show nphil) ++ "-1;\n   cs" ++ (show n)
  ++":=cs" ++ (show n) ++ "-1\n  };\n  st" ++ (show nphil)
  ++":=0;\n  while(st" ++ (show nphil)
  ++"==0) do{\n   st" ++ (show nphil)
  ++":=1 or skip  \n  };\n   eat" ++ (show nphil)
  ++":=eat" ++ (show nphil)
  ++"+1;\n   cs" ++ (show nphil)
  ++":=cs" ++ (show nphil) ++"+1;\n   cs" ++ (show n)
  ++":=cs" ++ (show n) ++ "+1\n }"


stDinPhil :: Int -> String
stDinPhil  n = let init1 = [ "(" ++ var ++ show id ++ "," ++ show 1 ++ ")" | var <- ["st", "cs"], id <- [1 .. n]]
                   init0 = [ "(" ++ var ++ show id ++ "," ++ show 0 ++ ")" | var <- ["eat"], id <- [1 .. n]]
                   init = init1 ++ init0
               in "[" ++ stDinPhilAux init ++ "]"
                  
stDinPhilAux :: [String] -> String
stDinPhilAux [] = ""
stDinPhilAux [h] = h
stDinPhilAux (h:t) = h ++ "," ++ stDinPhilAux t


lazy :: Int -> Int -> Int -> String
lazy k nphil n =
  "---DinPhil" ++ show n ++ "---\n"
  ++"hist: (1, [])\n"
  ++"k: " ++ show k
  ++"\n<\n"
  ++ (dinPhil nphil n)
  ++",\n "
  ++ (stDinPhil n)
  ++"\n>\n---DinPhil" ++ show n ++ "---\n"  

