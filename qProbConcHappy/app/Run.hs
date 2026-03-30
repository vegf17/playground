module Run where

import KStep
--import HistogramSem
--import ParserCom
--import ParserFile
import Syntax
import Examples
import Beautify
import User_Gates
import Com
import Collect_Samples
import JSONCodify
import GUI

import System.Exit
import Data.List
import Data.Matrix
import Data.Complex

import qualified Data.ByteString.Lazy as BL
import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

--Receives a filename and a scheduler, defined by the user, and executes runKStepSch for each
--program inside the filename
-- runSem :: String -> Sch -> IO()
-- runSem path sch = do
--   fileContent <- readFile path
--   case testsplitFile fileContent of
--     Left err -> print err  -- Print error if parsing fails
--     Right (ud, configs) -> do
--       writeFile "./user_gates.txt" ("user_defined_gates = [\n\t" ++ (toFileFormat ud) ++ "]")
--       runSemAux configs sch

-- toFileFormat :: [(String, [[Complex Double]])] -> String
-- toFileFormat [] = ""
-- toFileFormat ((g_name, g_m):[]) = "(" ++ show (g_name) ++ ", fromLists " ++ show (g_m) ++ ")"
-- toFileFormat ((g_name, g_m):t) = "(" ++ show (g_name) ++ ", fromLists" ++ show (g_m) ++ ")\n" ++ toFileFormat t 


-- runSem :: String -> Sch -> IO()
-- runSem path sch = do
--   fileContent <- readFile path
--   case parseRun fileContent of
--     Left err -> print err  -- Print error if parsing fails
--     Right configs -> runSemAux configs sch

runSem :: String -> Sch -> IO()
runSem path sch = do
  fileContent <- readFile path
  let configs = testFile fileContent
  runSemAux configs sch
    

runSemAux ::  [((String, Int, Int), (C,StC,L,StQ))] -> Sch -> IO()
runSemAux [] _ = return ()
runSemAux (((name, rep, k),(c,sc,l,sq)):t) sch = do
  let result = runKStepSch sch c (sc, l, sq) k
  putStrLn $ showRun (name, result)
  runSemAux t sch


--Receives a filename and shows a Histogram for each program inside the filename
runHist :: String -> Sch -> IO()
runHist path sch = do
  json_file <- prepareJsonFile path
  resetJsonFile json_file
  fileContent <- readFile path
  let configs = testFile fileContent
      config_GUI = defaultConfig
        {
          jsStatic = Just "app/static"
        }
  runHistAux configs sch json_file
  startGUI config_GUI (setup json_file)

runHistAux :: ListProgInfoFile -> Sch -> FilePath -> IO ()
runHistAux [] _ _ = return ()
runHistAux (h:t) sch json_file = do
  prog1 <- collectSamples h sch -- (String, [(Int, (StC, StQ))])
  --putStrLn $ showCollectSamples prog1
  appendJson json_file prog1
  runHistAux t sch json_file -- [(String, [(Int, (StC, StQ))])]
  
  
