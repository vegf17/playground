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
getCVariables (_, _, samples) =
  nub [ var | (_, (stc, _)) <- samples, (var, _) <- stc ]

extractCVarValues :: String -> SampleCollection -> [Double]
extractCVarValues varName (_, _, samples) =
  [ val | (_, (stc, _)) <- samples, (var, val) <- stc, var == varName ]

frequency :: [Double] -> [(Double, Int)]
frequency xs = M.toList $ M.fromListWith (+) [(x, 1) | x <- xs]

findProgram :: String -> [SampleCollection] -> SampleCollection
findProgram name = head . filter (\(n, _, _) -> n == name)

drawBarChart :: String -> [Double] -> UI ()
drawBarChart varName values = do
  let freqs  = frequency values
      xs     = map (show . fst) freqs
      ys     = map snd freqs
      xsJson = BL8.unpack (Aeson.encode xs)
      ysJson = BL8.unpack (Aeson.encode ys)

  runFunction $ ffi
    "(function () {\
    \  var plotDiv = document.getElementById('plot');\
    \  if (!plotDiv) {\
    \    console.error('plot div not found');\
    \    return;\
    \  }\
    \  plotDiv.innerHTML = 'Rendering chart...';\
    \  if (!window.Plotly) {\
    \    console.error('Plotly not loaded');\
    \    plotDiv.innerHTML = 'Plotly not loaded';\
    \    return;\
    \  }\
    \  var x = JSON.parse(%1);\
    \  var y = JSON.parse(%2);\
    \  console.log('x =', x);\
    \  console.log('y =', y);\
    \  var data = [{\
    \    x: x,\
    \    y: y,\
    \    type: 'bar',\
    \    marker: { color: '#4a90e2' }\
    \  }];\
    \  var layout = {\
    \    title: 'Distribution of ' + %3,\
    \    xaxis: { title: %3, type: 'category' },\
    \    yaxis: { title: 'Frequency' }\
    \  };\
    \  Plotly.newPlot(plotDiv, data, layout);\
    \})()" xsJson ysJson varName

waitForPlotlyAndDraw :: String -> [Double] -> UI ()
waitForPlotlyAndDraw varName values = do
  let freqs  = frequency values
      xs     = map (show . fst) freqs
      ys     = map snd freqs
      xsJson = BL8.unpack (Aeson.encode xs)
      ysJson = BL8.unpack (Aeson.encode ys)

  runFunction $ ffi
    "(function () {\
    \  function renderPlot() {\
    \    var plotDiv = document.getElementById('plot');\
    \    if (!plotDiv) {\
    \      console.error('plot div not found');\
    \      return;\
    \    }\
    \    plotDiv.innerHTML = '';\
    \    var x = JSON.parse(%1);\
    \    var y = JSON.parse(%2);\
    \    var name = %3;\
    \    var data = [{\
    \      x: x,\
    \      y: y,\
    \      type: 'bar',\
    \      marker: { color: '#4a90e2' }\
    \    }];\
    \    var layout = {\
    \      title: 'Distribution of ' + name,\
    \      xaxis: { title: name, type: 'category' },\
    \      yaxis: { title: 'Frequency' }\
    \    };\
    \    Plotly.newPlot(plotDiv, data, layout);\
    \  }\
    \  function waitForPlotly() {\
    \    var plotDiv = document.getElementById('plot');\
    \    if (window.Plotly) {\
    \      renderPlot();\
    \    } else {\
    \      if (plotDiv) plotDiv.innerHTML = 'Waiting for Plotly...';\
    \      setTimeout(waitForPlotly, 50);\
    \    }\
    \  }\
    \  waitForPlotly();\
    \})()" xsJson ysJson varName

    
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

  titleEl <- UI.h1
    #+ [string "Bar Chart Viewer"]
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
    # set text "Initializing..."
    # set UI.style
        [ ("border", "2px solid black")
        , ("height", "400px")
        , ("margin-top", "20px")
        , ("width", "100%")
        ]

  controls <- UI.div #+
    [ string "Program: ", element programSelect
    , UI.br
    , string "Classical variable: ", element cvarSelect
    , UI.br
    , string "Quantum variable: ", element qvarSelect
    ]

  container <- UI.div #+
    [ element titleEl
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
          then waitForPlotlyAndDraw "" []
          else waitForPlotlyAndDraw cvar (extractCVarValues cvar sc)

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
