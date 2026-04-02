{-# LANGUAGE OverloadedStrings #-}

module GUI where

import Prelude hiding (div)
import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.List
import Data.Matrix
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as M

import Collect_Samples
import JSONCodify
import Syntax
import Examples
import Beautify
import QuantumCalc

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

getQVariables :: SampleCollection -> [String]
getQVariables (_, l, _) = map fst l

getQSt :: SampleCollection -> (L,[StQ])
getQSt (_, l, samples) = let list_qst = [ qst | (_, (_, qst)) <- samples ]
                         in (l, list_qst)

extractQSt :: QVarList -> SampleCollection -> [String]
extractQSt qvars sample_collection = let (l, list_qst) = getQSt sample_collection
                                         id_qvars = qNums qvars l
                                         nqubits = length l
                                         list_reduced_qst = map (partialTraceMany nqubits id_qvars) list_qst
                                     in map qStToString list_reduced_qst


-- get the reduced density operator for given quantum variables
getQStQVars :: QVarList -> SampleCollection -> [StQ]
getQStQVars list_qvars sample_collection =
  let (l, list_qst) = getQSt sample_collection
      id_qvars      = [id-1 | (q, id) <- l, (elem q list_qvars==False)]
      nqubits       = length l
  in map (partialTraceMany nqubits id_qvars) list_qst



extractQVarValues :: String -> SampleCollection -> [String]
extractQVarValues qvarName sample_collection =
  map qStToString (getQStQVars [qvarName] sample_collection)                                        

qStToString :: StQ -> String
qStToString = denOpToKetBraComplex

frequencyStr :: [String] -> [(String, Int)]
frequencyStr xs = M.toList $ M.fromListWith (+) [(x, 1) | x <- xs]


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

waitForPlotlyAndDraw :: String -> [String] -> UI ()
waitForPlotlyAndDraw varName values = do
  let freqs  = frequencyStr values
      xs     = map fst freqs
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
    # set (attr "size") "5"
    # set UI.style
        [ ("width", "180px")
        , ("height", "120px")
        , ("overflow-y", "auto")
        ]

  element programSelect #+ map (\x -> UI.option #+ [string x]) programNames
  case programNames of
    (p:_) -> element programSelect # set value p >> return ()
    []    -> return ()

  cvarSelect <- UI.select
    # set (attr "size") "5"
    # set UI.style
        [ ("width", "180px")
        , ("height", "120px")
        , ("overflow-y", "auto")
        ]

  let varsClassic =
        case programNames of
          [] -> ["<None>"]
          (p:_) ->
            let xs = getCVariables (findProgram p collections)
            in "<None>" : xs

  element cvarSelect #+ map (\x -> UI.option #+ [string x]) varsClassic
  case varsClassic of
    (v:_) -> element cvarSelect # set value v >> return ()
    []    -> return ()

  qvarSelect <- UI.select
    # set (attr "size") "5"
    # set UI.style
        [ ("width", "180px")
        , ("height", "120px")
        , ("overflow-y", "auto")
        ]

  let varsQuantum =
        case programNames of
          [] -> ["<None>"]
          (p:_) ->
            let xs = getQVariables (findProgram p collections)
            in "<None>" : xs

  element qvarSelect #+ map (\x -> UI.option #+ [string x]) varsQuantum
  case varsQuantum of
    (q:_) -> element qvarSelect # set value q >> return ()
    []    -> return ()

  barChartBox <- UI.div
    # set UI.id_ "plot"
    # set text "Initializing..."
    # set UI.style
        [ ("border", "2px solid black")
        , ("height", "400px")
        , ("width", "100%")
        ]

  programPanel <- UI.div #+
    [ UI.div #+
        [ string "Programs:" ]
        # set UI.style [("margin-bottom", "8px"), ("font-weight", "bold")]
    , element programSelect
    ]
    # set UI.style
        [ ("margin-bottom", "20px") ]

  cvarPanel <- UI.div #+
    [ UI.div #+
        [ string "Classical variables:" ]
        # set UI.style [("margin-bottom", "8px"), ("font-weight", "bold")]
    , element cvarSelect
    ]
    # set UI.style
        [ ("margin-bottom", "20px") ]

  qvarPanel <- UI.div #+
    [ UI.div #+
        [ string "Quantum variables:" ]
        # set UI.style [("margin-bottom", "8px"), ("font-weight", "bold")]
    , element qvarSelect
    ]

  controlsPanel <- UI.div #+
    [ element programPanel
    , element cvarPanel
    , element qvarPanel
    ]
    # set UI.style
        [ ("width", "200px")
        , ("margin-right", "20px")
        , ("flex-shrink", "0")
        ]

  chartArea <- UI.div #+
    [ element barChartBox ]
    # set UI.style
        [ ("flex-grow", "1") ]

  middleRow <- UI.div #+
    [ element controlsPanel
    , element chartArea
    ]
    # set UI.style
        [ ("display", "flex")
        , ("align-items", "flex-start")
        ]

  container <- UI.div #+
    [ element titleEl
    , element middleRow
    ]
    # set UI.style
        [ ("width", "900px")
        , ("margin", "30px auto")
        ]

  getBody window #+ [element container]

  let updateBarChart = do
        prog <- get value programSelect
        cvar <- get value cvarSelect
        qvar <- get value qvarSelect
        let sc = findProgram prog collections

        if qvar /= "<None>"
          then waitForPlotlyAndDraw qvar (extractQVarValues qvar sc)
          else if cvar /= "<None>"
            then waitForPlotlyAndDraw cvar (map show (extractCVarValues cvar sc))
            else waitForPlotlyAndDraw "" []

  on UI.selectionChange programSelect $ \_ -> do
    prog <- get value programSelect
    let sc = findProgram prog collections

    let cvars = "<None>" : getCVariables sc
    element cvarSelect # set children []
    element cvarSelect #+ map (\v -> UI.option #+ [string v]) cvars
    case cvars of
      (v:_) -> element cvarSelect # set value v >> return ()
      []    -> return ()

    let qvars = "<None>" : getQVariables sc
    element qvarSelect # set children []
    element qvarSelect #+ map (\q -> UI.option #+ [string q]) qvars
    case qvars of
      (q:_) -> element qvarSelect # set value q >> return ()
      []    -> return ()

    updateBarChart

  on UI.selectionChange cvarSelect $ \_ -> do
    element qvarSelect # set value "<None>"
    updateBarChart

  on UI.selectionChange qvarSelect $ \_ -> do
    element cvarSelect # set value "<None>"
    updateBarChart

  updateBarChart

-- waitForPlotlyAndDraw :: String -> [Double] -> UI ()
-- waitForPlotlyAndDraw varName values = do
--   let freqs  = frequency values
--       xs     = map (show . fst) freqs
--       ys     = map snd freqs
--       xsJson = BL8.unpack (Aeson.encode xs)
--       ysJson = BL8.unpack (Aeson.encode ys)

--   runFunction $ ffi
--     "(function () {\
--     \  function renderPlot() {\
--     \    var plotDiv = document.getElementById('plot');\
--     \    if (!plotDiv) {\
--     \      console.error('plot div not found');\
--     \      return;\
--     \    }\
--     \    plotDiv.innerHTML = '';\
--     \    var x = JSON.parse(%1);\
--     \    var y = JSON.parse(%2);\
--     \    var name = %3;\
--     \    var data = [{\
--     \      x: x,\
--     \      y: y,\
--     \      type: 'bar',\
--     \      marker: { color: '#4a90e2' }\
--     \    }];\
--     \    var layout = {\
--     \      title: 'Distribution of ' + name,\
--     \      xaxis: { title: name, type: 'category' },\
--     \      yaxis: { title: 'Frequency' }\
--     \    };\
--     \    Plotly.newPlot(plotDiv, data, layout);\
--     \  }\
--     \  function waitForPlotly() {\
--     \    var plotDiv = document.getElementById('plot');\
--     \    if (window.Plotly) {\
--     \      renderPlot();\
--     \    } else {\
--     \      if (plotDiv) plotDiv.innerHTML = 'Waiting for Plotly...';\
--     \      setTimeout(waitForPlotly, 50);\
--     \    }\
--     \  }\
--     \  waitForPlotly();\
--     \})()" xsJson ysJson varName



-- setup :: FilePath -> Window -> UI ()
-- setup json_file window = do
--   return window # set title "Histogram UI"

--   headEl <- getHead window

--   plotlyScript <- mkElement "script"
--     # set (attr "src") "/static/plotly.min.js"
--     # set (attr "type") "text/javascript"

--   element headEl #+ [element plotlyScript]

--   collections <- liftIO $ loadCollections json_file
--   let programNames = getProgramNames collections

--   titleEl <- UI.h1
--     #+ [string "Bar Chart Viewer"]
--     # set UI.style [("text-align", "center")]

--   programSelect <- UI.select
--     # set (attr "size") "5"
--     # set UI.style
--         [ ("width", "180px")
--         , ("height", "120px")
--         , ("overflow-y", "auto")
--         ]

--   element programSelect #+ map (\x -> UI.option #+ [string x]) programNames
--   case programNames of
--     (p:_) -> element programSelect # set value p >> return ()
--     []    -> return ()

--   cvarSelect <- UI.select
--     # set (attr "size") "5"
--     # set UI.style
--         [ ("width", "180px")
--         , ("height", "120px")
--         , ("overflow-y", "auto")
--         ]

--   let varsClassic =
--         case programNames of
--           [] -> ["<None>"]
--           (p:_) ->
--             case getCVariables (findProgram p collections) of
--               [] -> ["<None>"]
--               xs -> xs

--   element cvarSelect #+ map (\x -> UI.option #+ [string x]) varsClassic
--   case varsClassic of
--     (v:_) -> element cvarSelect # set value v >> return ()
--     []    -> return ()

--   qvarSelect <- UI.select
--     # set (attr "size") "5"
--     # set UI.style
--         [ ("width", "180px")
--         , ("height", "120px")
--         , ("overflow-y", "auto")
--         ]

--   let varsQuantum =
--         case programNames of
--           [] -> ["<None>"]
--           (p:_) ->
--             case getQVariables (findProgram p collections) of
--               [] -> ["<None>"]
--               xs -> xs

--   element qvarSelect #+ map (\x -> UI.option #+ [string x]) varsQuantum
--   case varsQuantum of
--     (q:_) -> element qvarSelect # set value q >> return ()
--     []    -> return ()

--   barChartBox <- UI.div
--     # set UI.id_ "plot"
--     # set text "Initializing..."
--     # set UI.style
--         [ ("border", "2px solid black")
--         , ("height", "400px")
--         , ("width", "100%")
--         ]

--   programPanel <- UI.div #+
--     [ UI.div #+
--         [ string "Programs:" ]
--         # set UI.style [("margin-bottom", "8px"), ("font-weight", "bold")]
--     , element programSelect
--     ]
--     # set UI.style
--         [ ("margin-bottom", "20px") ]

--   cvarPanel <- UI.div #+
--     [ UI.div #+
--         [ string "Classical variables:" ]
--         # set UI.style [("margin-bottom", "8px"), ("font-weight", "bold")]
--     , element cvarSelect
--     ]
--     # set UI.style
--         [ ("margin-bottom", "20px") ]

--   qvarPanel <- UI.div #+
--     [ UI.div #+
--         [ string "Quantum variables:" ]
--         # set UI.style [("margin-bottom", "8px"), ("font-weight", "bold")]
--     , element qvarSelect
--     ]

--   controlsPanel <- UI.div #+
--     [ element programPanel
--     , element cvarPanel
--     , element qvarPanel
--     ]
--     # set UI.style
--         [ ("width", "200px")
--         , ("margin-right", "20px")
--         , ("flex-shrink", "0")
--         ]

--   chartArea <- UI.div #+
--     [ element barChartBox ]
--     # set UI.style
--         [ ("flex-grow", "1") ]

--   middleRow <- UI.div #+
--     [ element controlsPanel
--     , element chartArea
--     ]
--     # set UI.style
--         [ ("display", "flex")
--         , ("align-items", "flex-start")
--         ]

--   container <- UI.div #+
--     [ element titleEl
--     , element middleRow
--     ]
--     # set UI.style
--         [ ("width", "900px")
--         , ("margin", "30px auto")
--         ]

--   getBody window #+ [element container]

--   let updateBarChart = do
--         prog <- get value programSelect
--         cvar <- get value cvarSelect
--         qvar <- get value qvarSelect
--         let sc = findProgram prog collections
--         if cvar == "<None>"
--           then waitForPlotlyAndDraw "" []
--           else waitForPlotlyAndDraw cvar (extractCVarValues cvar sc)

--   on UI.selectionChange programSelect $ \_ -> do
--     prog <- get value programSelect

--     let vars =
--           case getCVariables (findProgram prog collections) of
--             [] -> ["<None>"]
--             xs -> xs

--     element cvarSelect # set children []
--     element cvarSelect #+ map (\v -> UI.option #+ [string v]) vars

--     case vars of
--       (v:_) -> element cvarSelect # set value v >> return ()
--       []    -> return ()

--     updateBarChart

--   on UI.selectionChange cvarSelect $ const updateBarChart
--   on UI.selectionChange qvarSelect $ const updateBarChart

--   updateBarChart



-- Work for the partial trace ---
removeAt :: Int -> [a] -> [a]
removeAt i xs = take i xs ++ drop (i + 1) xs

insertAt :: Int -> a -> [a] -> [a]
insertAt i x xs = take i xs ++ [x] ++ drop i xs

-- Binary multi-indices of length n
allBitStrings :: Int -> [[Int]]
allBitStrings 0 = [[]]
allBitStrings n = [b : bs | b <- [0,1], bs <- allBitStrings (n-1)]

-- Row-major flattening of a bitstring
-- e.g. [1,0,1] -> 5
flattenBits :: [Int] -> Int
flattenBits = foldl' (\acc b -> 2 * acc + b) 0

-- Trace out one qubit (0-based)
{-
n: number of qubits in the system
q: qubit to trace-out
rho: quantum state
-}
partialTrace1 :: Int -> Int -> StQ -> StQ
partialTrace1 n q rho
  | q < 0 || q >= n =
      error "partialTrace1: invalid qubit index"
  | nrows rho /= dim || ncols rho /= dim =
      error "partialTrace1: matrix size does not match number of qubits"
  | otherwise =
      matrix dim' dim' entry
  where
    dim  = 2 ^ n
    dim' = 2 ^ (n - 1)
    inds = allBitStrings (n - 1)

    entry (r, c) =
      let alpha = inds !! (r - 1)
          beta  = inds !! (c - 1)
      in sum
           [ getElem (flattenBits (insertAt q b alpha) + 1)
                     (flattenBits (insertAt q b beta)  + 1)
                     rho
           | b <- [0,1]
           ]

-- Trace out several qubits (0-based)
{-
n: number of qubits in the system
qs: list of qubits to trace-out
rho: quantum state
-}
partialTraceMany :: Int -> [Int] -> StQ -> StQ
partialTraceMany n qs rho
  | any (\q -> q < 0 || q >= n) qs =
      error "partialTraceMany: invalid qubit index"
  | nrows rho /= dim || ncols rho /= dim =
      error "partialTraceMany: matrix size does not match number of qubits"
  | otherwise =
      fst $ foldl' step (rho, n) sortedQs
  where
    dim = 2 ^ n

    -- descending order is important
    sortedQs = reverse (sort (nub qs))

    step :: (StQ, Int) -> Int -> (StQ, Int)
    step (currentRho, currentN) q =
      ( partialTrace1 currentN q currentRho
      , currentN - 1
      )
