{-# LANGUAGE OverloadedStrings #-}

module GUI where

import Prelude hiding (div)
import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.List

import Collect_Samples
import JSONCodify

--main :: IO ()
--main = startGUI defaultConfig setup 

loadCollections :: FilePath -> IO [SampleCollection]
loadCollections path = do
  content <- BL8.readFile path
  let ls = BL8.lines content
  return $ map decodeLine ls
  where
    decodeLine l =
      case decodeSampleCollection l of
        Right sc -> sc
        Left err -> error err

getProgramNames :: [SampleCollection] -> [String]
getProgramNames = map fst

getCVariables :: SampleCollection -> [String]
getCVariables (_, samples) = nub [ var | (_, (stc, _)) <- samples, (var, _) <- stc ]

{-
NEED TO THINK ABOUT THE INFORMATION REGARDING THE QUANTUM VARIABLES
-}

findProgram :: String -> [SampleCollection] -> SampleCollection
findProgram name = head . filter (\(n, _) -> n == name)

setup :: FilePath -> Window -> UI ()
setup json_file window = do
  return window # set title "Histogram UI"

  ----------------------------------------
  -- Load data
  ----------------------------------------
  collections <- liftIO $ loadCollections json_file

  let programNames = getProgramNames collections

  ----------------------------------------
  -- Title
  ----------------------------------------
  title <- UI.h1 #+ [string "Histogram Viewer"]
                 # set UI.style [("text-align", "center")]

  ----------------------------------------
  -- Program dropdown
  ----------------------------------------
  programSelect <- UI.select
  element programSelect #+ map (\x -> UI.option #+ [string x]) programNames

  ----------------------------------------
  -- Classical variable dropdown
  ----------------------------------------
  cvarSelect <- UI.select
  
  let varsClassic = case programNames of
                      [] -> []
                      (p:_) -> let vars = getCVariables (findProgram p collections)
                               in vars
  
  element cvarSelect #+ map (\x -> UI.option #+ [string x]) varsClassic

  ----------------------------------------
  -- Update variables when program changes
  ----------------------------------------
  on UI.selectionChange programSelect $ \_ -> do
    prog <- get value programSelect

    let vars = case getCVariables (findProgram prog collections) of
          [] -> ["<None>"]
          x -> x

    -- clear old options
    element cvarSelect # set children []

    -- add new options
    element cvarSelect #+
      map (\v -> UI.option #+ [string v]) vars  
  
  ----------------------------------------
  -- Quantum variable dropdown (placeholder)
  ----------------------------------------
  qvarSelect <- UI.select
  element qvarSelect #+ map (\x -> UI.option #+ [string x]) ["q1", "q2"]

  ----------------------------------------
  -- Histogram area
  ----------------------------------------
  histogramBox <- UI.div
    # set UI.style
        [ ("border", "2px solid black")
        , ("height", "400px")
        , ("margin-top", "20px")
        , ("display", "flex")
        , ("align-items", "center")
        , ("justify-content", "center")
        ]
    #+ [string "Histogram will appear here"]

  ----------------------------------------
  -- Snapshot button
  ----------------------------------------
  snapshotBtn <- UI.button
    #+ [string "Take snapshot"]
    # set UI.style [("margin-top", "20px")]

  ----------------------------------------
  -- Layout
  ----------------------------------------
  controls <- UI.div #+ 
    [ string "Program: ", element programSelect
    , UI.br
    , string "Classical variable: ", element cvarSelect
    , UI.br
    , string "Quantum variable: ", element qvarSelect
    ]

  container <- UI.div #+
    [ element title
    , element controls
    , element histogramBox
    , element snapshotBtn
    ]
    # set UI.style
        [ ("width", "800px")
        , ("margin", "30px auto")
        ]

  getBody window #+ [element container]

  ----------------------------------------
  -- EVENTS
  ----------------------------------------

  let updateHistogram = do
        prog <- get value programSelect
        cvar <- get value cvarSelect
        qvar <- get value qvarSelect

        txt <- string ("Program: " ++ prog ++ " | CVar: " ++ cvar ++ " | QVar: " ++ qvar)
      
        element histogramBox # set children [txt]

  on UI.selectionChange programSelect $ const updateHistogram
  on UI.selectionChange cvarSelect    $ const updateHistogram
  on UI.selectionChange qvarSelect    $ const updateHistogram

  ----------------------------------------
  -- Snapshot button
  ----------------------------------------
  on UI.click snapshotBtn $ \_ -> do
    liftIO $ putStrLn "Snapshot requested!"
