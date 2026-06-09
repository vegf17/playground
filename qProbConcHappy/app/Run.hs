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
import Debug

import JSONDebug
import GuiDebug

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
  

-- Receives a filename and produces the debugger GUI.
-- It stores the result of run_debug_KStepSch in a JSON file and then opens
-- a Threepenny GUI where the user can select:
--   program -> outcome -> step
runDebug :: String -> SchDebug -> IO ()
runDebug path sch_d = do
  json_file <- prepareDebugJsonFile path
  resetDebugJsonFile json_file

  fileContent <- readFile path

  let configs = testFile fileContent
      config_GUI = defaultConfig
        { jsStatic = Just "app/static"
        }

  runDebugAux configs sch_d json_file
  startGUI config_GUI (setupDebug json_file)


runDebugAux
  :: [((String, Int, Int), (C, StC, L, StQ))]
  -> SchDebug
  -> FilePath
  -> IO ()
runDebugAux [] _ _ = return ()
runDebugAux (((name, _rep, k), (c, sc, l, sq)) : t) sch_d json_file = do
  let initial = (sc, l, sq)
      result  = run_debug_KStepSch sch_d c initial k
      payload = debugResultToJSON name initial result

  appendDebugJson json_file payload
  runDebugAux t sch_d json_file
  
-- --Receives a filename and produces the debugger
-- runDebug :: String -> SchDebug -> IO()
-- runDebug path sch_d = do
--   fileContent <- readFile path
--   let configs = testFile fileContent
--   runDebugAux configs sch_d
    

-- runDebugAux ::  [((String, Int, Int), (C,StC,L,StQ))] -> SchDebug -> IO()
-- runDebugAux [] _ = return ()
-- runDebugAux (((name, rep, k),(c,sc,l,sq)):t) sch_d = do
--   let result = run_debug_KStepSch sch_d c (sc, l, sq) k
--   putStrLn name
--   show_debug_KStepSch sch_d c (sc, l, sq) k
--   --putStrLn $ show (length result)
--   --putStrLn $ show result
--   --putStrLn $ showRun (name, result)
--   runDebugAux t sch_d

