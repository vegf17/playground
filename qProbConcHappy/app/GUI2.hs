{-# LANGUAGE OverloadedStrings #-}

module GUI where

import Prelude hiding (div)
import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.List
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as M

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
getProgramNames = map (\(name, _, _) -> name)

getCVariables :: SampleCollection -> [String]
getCVariables (_, _, samples) = nub [ var | (_, (stc, _)) <- samples, (var, _) <- stc ]

extractCVarValues :: String -> SampleCollection -> [Double]
extractCVarValues varName (_, _, samples) = [ val | (_, (stc, _)) <- samples, (var, val) <- stc, var == varName ]

{-
NEED TO THINK ABOUT THE INFORMATION REGARDING THE QUANTUM VARIABLES
-}


frequency :: [Double] -> [(Double, Int)]
frequency xs = M.toList $ M.fromListWith (+) [(x, 1) | x <- xs]

drawBarChart :: String -> [Double] -> UI ()
drawBarChart varName values = do
  let freqs = frequency values

  let xs = map (show . fst) freqs
  let ys = map snd freqs

  let xsJson = BL8.unpack (Aeson.encode xs)
  let ysJson = BL8.unpack (Aeson.encode ys)

  runFunction $ ffi "var x = JSON.parse(%1); \
                    \var y = JSON.parse(%2); \
                    \var data = [{ \
                    \  x: x, \
                    \  y: y, \
                    \  type: 'bar', \
                    \  marker: { color: '#4a90e2' } \
                    \}]; \
                    \var layout = { \
                    \  title: 'Distribution of ' + %3, \
                    \  xaxis: { title: %3, type: 'category' }, \
                    \  yaxis: { title: 'Frequency' } \
                    \}; \
                    \if (window.Plotly) { \
                    \  Plotly.newPlot('plot', data, layout); \
                    \} else { \
                    \  console.error('Plotly not loaded yet'); \
                    \}" xsJson ysJson varName

-- drawBarChart :: String -> [Double] -> UI ()
-- drawBarChart varName values = do
--   let freqs = frequency values

--   let xs = map fst freqs
--   let ys = map snd freqs

--   let xsJson = BL8.unpack (Aeson.encode xs)
--   let ysJson = BL8.unpack (Aeson.encode ys)

--   runFunction $ ffi "var x = JSON.parse(%1); \
--                     \var y = JSON.parse(%2); \
--                     \var data = [{ \
--                     \  x: x, \
--                     \  y: y, \
--                     \  type: 'bar', \
--                     \  marker: { color: '#4a90e2' } \
--                     \}]; \
--                     \var layout = { \
--                     \  title: 'Distribution of ' + %3, \
--                     \  xaxis: { title: %3, type: 'category' }, \
--                     \  yaxis: { title: 'Frequency' } \
--                     \}; \
--                     \Plotly.react('plot', data, layout);" xsJson ysJson varName

-- drawHistogram :: String -> Double -> [Double] -> UI ()
-- drawHistogram varName binSize values = do
--   let valuesJson = BL8.unpack (Aeson.encode values)

--   runFunction $ ffi "var xs = JSON.parse(%1); \
--                     \var data = [{ \
--                     \  x: xs, \
--                     \  type: 'histogram', \
--                     \  xbins: { size: %2 }, \
--                     \  marker: { color: '#4a90e2' } \
--                     \}]; \
--                     \var layout = { \
--                     \  title: 'Histogram of ' + %3, \
--                     \  xaxis: { title: %3 }, \
--                     \  yaxis: { title: 'Frequency' } \
--                     \}; \
--                     \Plotly.react('plot', data, layout);" valuesJson binSize varName


findProgram :: String -> [SampleCollection] -> SampleCollection
findProgram name = head . filter (\(n, _, _) -> n == name)

setup :: FilePath -> Window -> UI ()
setup json_file window = do
  return window # set title "Histogram UI"

  headEl <- getHead window

  plotlyScript <- mkElement "script"
    # set (attr "src") "/static/plotly.min.js"
    # set (attr "type") "text/javascript"

  element headEl #+ [element plotlyScript]

  collections <- liftIO $ loadCollections json_file
  let programNames = getProgramNames collections

  title <- UI.h1 #+ [string "Bar Chart Viewer"]
                 # set UI.style [("text-align", "center")]

  programSelect <- UI.select
  element programSelect #+ map (\x -> UI.option #+ [string x]) programNames
  case programNames of
    (p:_) -> element programSelect # set value p >> return ()
    []    -> return ()

  cvarSelect <- UI.select
  let varsClassic =
        case programNames of
          [] -> ["<None>"]
          (p:_) ->
            case getCVariables (findProgram p collections) of
              [] -> ["<None>"]
              xs -> xs

  element cvarSelect #+ map (\x -> UI.option #+ [string x]) varsClassic
  case varsClassic of
    (v:_) -> element cvarSelect # set value v >> return ()
    []    -> return ()

  qvarSelect <- UI.select
  element qvarSelect #+ map (\x -> UI.option #+ [string x]) ["q1", "q2"]

  barChartBox <- UI.div
    # set UI.id_ "plot"
    # set UI.style
        [ ("border", "2px solid black")
        , ("height", "400px")
        , ("margin-top", "20px")
        ]

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
    , element barChartBox
    ]
    # set UI.style
        [ ("width", "800px")
        , ("margin", "30px auto")
        ]

  getBody window #+ [element container]

  let updateBarChart = do
        prog <- get value programSelect
        cvar <- get value cvarSelect
        let sc = findProgram prog collections
        if cvar == "<None>"
          then drawBarChart "" []
          else drawBarChart cvar (extractCVarValues cvar sc)

  on UI.selectionChange programSelect $ \_ -> do
    prog <- get value programSelect
    let vars =
          case getCVariables (findProgram prog collections) of
            [] -> ["<None>"]
            xs -> xs

    element cvarSelect # set children []
    element cvarSelect #+ map (\v -> UI.option #+ [string v]) vars

    case vars of
      (v:_) -> element cvarSelect # set value v >> return ()
      []    -> return ()

    updateBarChart

  on UI.selectionChange cvarSelect $ const updateBarChart
  on UI.selectionChange qvarSelect $ const updateBarChart

  updateBarChart


-- setup :: FilePath -> Window -> UI ()
-- setup json_file window = do
--   runFunction $ ffi "var s = document.createElement('script'); \
--                     \s.src = 'https://cdn.plot.ly/plotly-latest.min.js'; \
--                     \document.head.appendChild(s);"

--   return window # set title "Histogram UI"

--   ----------------------------------------
--   -- Load data
--   ----------------------------------------
--   collections <- liftIO $ loadCollections json_file

--   let programNames = getProgramNames collections

--   ----------------------------------------
--   -- Title
--   ----------------------------------------
--   title <- UI.h1 #+ [string "Histogram Viewer"]
--                  # set UI.style [("text-align", "center")]

--   ----------------------------------------
--   -- Program dropdown
--   ----------------------------------------
--   programSelect <- UI.select
--   element programSelect #+ map (\x -> UI.option #+ [string x]) programNames

--   ----------------------------------------
--   -- Classical variable dropdown
--   ----------------------------------------
--   cvarSelect <- UI.select
  
--   let varsClassic = case programNames of
--                       [] -> []
--                       (p:_) -> let vars = getCVariables (findProgram p collections)
--                                in vars
  
--   element cvarSelect #+ map (\x -> UI.option #+ [string x]) varsClassic

--   ----------------------------------------
--   -- Update variables when program changes
--   ----------------------------------------
--   on UI.selectionChange programSelect $ \_ -> do
--     prog <- get value programSelect

--     let vars = case getCVariables (findProgram prog collections) of
--           [] -> ["<None>"]
--           x -> x

--     -- clear old options
--     element cvarSelect # set children []

--     -- add new options
--     element cvarSelect #+
--       map (\v -> UI.option #+ [string v]) vars  
  
--   ----------------------------------------
--   -- Quantum variable dropdown (placeholder)
--   ----------------------------------------
--   qvarSelect <- UI.select
--   element qvarSelect #+ map (\x -> UI.option #+ [string x]) ["q1", "q2"]

--   ----------------------------------------
--   -- Histogram area
--   ----------------------------------------
--   histogramBox <- UI.div
--     # set UI.id_ "plot"
--     # set UI.style
--         [ ("border", "2px solid black")
--         , ("height", "400px")
--         , ("margin-top", "20px")
--         , ("display", "flex")
--         , ("align-items", "center")
--         , ("justify-content", "center")
--         ]

--   ----------------------------------------
--   -- Layout
--   ----------------------------------------
--   controls <- UI.div #+ 
--     [ string "Program: ", element programSelect
--     , UI.br
--     , string "Classical variable: ", element cvarSelect
--     , UI.br
--     , string "Quantum variable: ", element qvarSelect
--     ]

--   container <- UI.div #+
--     [ element title
--     , element controls
--     , element histogramBox
--     ]
--     # set UI.style
--         [ ("width", "800px")
--         , ("margin", "30px auto")
--         ]

--   getBody window #+ [element container]

--   ----------------------------------------
--   -- EVENTS
--   ----------------------------------------

--   let updateHistogram = do
--         prog <- get value programSelect
--         cvar <- get value cvarSelect

--         let sc = findProgram prog collections

--         if cvar == "<None>"
--           then drawHistogram []
--           else
--           do
--             let values = extractCVarValues cvar sc

--             if null values
--               then drawHistogram []
--               else drawHistogram values

--   on UI.selectionChange programSelect $ const updateHistogram
--   on UI.selectionChange cvarSelect    $ const updateHistogram
--   on UI.selectionChange qvarSelect    $ const updateHistogram
