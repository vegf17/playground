{-# LANGUAGE OverloadedStrings #-}

module GuiDebug
  ( setupDebug
  , loadDebugCollections
  ) where

import Prelude hiding (div)
import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Char (isDigit)
import Numeric (showFFloat)

import JSONDebug

--------------------------------------------------------------------------------
-- Load newline-delimited debug JSON
--------------------------------------------------------------------------------

loadDebugCollections :: FilePath -> IO [DebugCollectionJSON]
loadDebugCollections path = do
  content <- BL8.readFile path
  case Aeson.eitherDecode content of
    Right xs  -> pure xs
    Left err  -> error err

-- loadDebugCollections :: FilePath -> IO [DebugCollectionJSON]
-- loadDebugCollections path = do
--   content <- BL8.readFile path
--   let nonEmptyLines = filter (not . BL8.null) (BL8.lines content)
--   pure $ map decodeLine nonEmptyLines
--   where
--     decodeLine l =
--       case decodeDebugCollection l of
--         Right x  -> x
--         Left err -> error err

--------------------------------------------------------------------------------
-- Small helpers
--------------------------------------------------------------------------------

findProgram :: String -> [DebugCollectionJSON] -> DebugCollectionJSON
findProgram name collections =
  fromMaybe (head collections) $
    find (\c -> programName c == name) collections

findOutcome :: Int -> DebugCollectionJSON -> DebugOutcomeJSON
findOutcome oid collection =
  fromMaybe (head (outcomes collection)) $
    find (\o -> outcomeId o == oid) (outcomes collection)

findStep :: Int -> DebugOutcomeJSON -> Maybe DebugStepJSON
findStep n outcome =
  find (\s -> stepNumber s == n) (steps outcome)

safeReadInt :: String -> Int
safeReadInt s =
  case reads s of
    [(n, "")] -> n
    _         -> 0

clamp :: Int -> Int -> Int -> Int
clamp lo hi x = max lo (min hi x)

--------------------------------------------------------------------------------
-- Limiting decimal numbers to 5 digits
--------------------------------------------------------------------------------

formatDouble5 :: Double -> String
formatDouble5 x =
  trimTrailingZeros raw
  where
    x' =
      if abs x < 0.000005
        then 0
        else x

    raw = showFFloat (Just 5) x' ""

trimTrailingZeros :: String -> String
trimTrailingZeros s =
  case break (== '.') s of
    (_, "") -> s
    (beforeDot, dotAndAfter) ->
      let afterDot = drop 1 dotAndAfter
          trimmedAfter = reverse (dropWhile (== '0') (reverse afterDot))
      in case trimmedAfter of
           "" -> beforeDot
           _  -> beforeDot ++ "." ++ trimmedAfter

-- This tries to round decimal numbers occurring in pretty-printed states,
-- while leaving integers such as |0>, |1>, q1, q2 mostly untouched.
limitDecimalsInText :: String -> String
limitDecimalsInText [] = []
limitDecimalsInText s@(c:cs)
  | isNumberStart c =
      case reads s :: [(Double, String)] of
        [(d, rest)] ->
          let consumed = take (length s - length rest) s
          in if shouldFormat consumed
               then formatDouble5 d ++ limitDecimalsInText rest
               else c : limitDecimalsInText cs
        _ -> c : limitDecimalsInText cs
  | otherwise = c : limitDecimalsInText cs
  where
    isNumberStart ch =
      isDigit ch || ch == '-' || ch == '.'

    shouldFormat token =
      '.' `elem` token || 'e' `elem` token || 'E' `elem` token

prettyProbability :: Double -> String
prettyProbability = formatDouble5

--------------------------------------------------------------------------------
-- GUI options
--------------------------------------------------------------------------------

setOptions :: Element -> [(String, String)] -> UI ()
setOptions select opts = do
  element select # set children []
  element select #+
    [ UI.option # set value val #+ [string label]
    | (val, label) <- opts
    ]
  case opts of
    ((val, _) : _) -> element select # set value val >> return ()
    []             -> return ()

outcomeOptions :: DebugCollectionJSON -> [(String, String)]
outcomeOptions collection =
  [ ( show (outcomeId o)
    , "Outcome "
        ++ show (outcomeId o)
        ++ " | p = "
        ++ prettyProbability (probability o)
    )
  | o <- outcomes collection
  ]

stepOptions :: DebugOutcomeJSON -> [(String, String)]
stepOptions outcome =
  ("0", "Initial state") :
  [ ( show (stepNumber s)
    , "Step " ++ show (stepNumber s) ++ ": " ++ action s
    )
  | s <- steps outcome
  ]

prettyLMem :: JLMem -> (String, String)
prettyLMem st =
  ( limitDecimalsInText (jlClassical st)
  , limitDecimalsInText (jlQuantum st)
  )

prettyMem :: JMem -> (String, String)
prettyMem st =
  ( limitDecimalsInText (jClassical st)
  , limitDecimalsInText (jQuantum st)
  )

preBlock :: UI Element
preBlock =
  UI.pre # set UI.style
    [ ("white-space", "pre-wrap")
    , ("overflow-x", "auto")
    , ("background", "#f7f7f7")
    , ("border", "1px solid #ddd")
    , ("border-radius", "6px")
    , ("padding", "10px")
    ]

sectionTitle :: String -> UI Element
sectionTitle txt = UI.h3 #+ [string txt]

navButton :: String -> UI Element
navButton label =
  UI.button #+ [string label]
    # set UI.style
        [ ("margin-left", "8px")
        , ("padding", "4px 10px")
        ]

--------------------------------------------------------------------------------
-- GUI
--------------------------------------------------------------------------------

setupDebug :: FilePath -> Window -> UI ()
setupDebug jsonFile window = do
  return window # set title "Debug Viewer"

  collections <- liftIO $ loadDebugCollections jsonFile

  case collections of
    [] -> do
      title <- UI.h1 #+ [string "Debug Viewer"]
      msg <- UI.p #+ [string "No debug data found in the JSON file."]
      getBody window #+ [element title, element msg]
      return ()

    _ -> do
      let firstCollection = head collections
          programOpts =
            [ (programName c, programName c)
            | c <- collections
            ]

      title <- UI.h1 #+ [string "Debug Viewer"]
               # set UI.style [("text-align", "center")]

      programSelect <- UI.select
      outcomeSelect <- UI.select
      stepSelect <- UI.select

      prevStepButton <- navButton "Previous step"
      nextStepButton <- navButton "Next step"

      setOptions programSelect programOpts
      setOptions outcomeSelect (outcomeOptions firstCollection)

      let firstOutcome =
            case outcomes firstCollection of
              []    -> Nothing
              (o:_) -> Just o

      case firstOutcome of
        Nothing -> setOptions stepSelect []
        Just o  -> setOptions stepSelect (stepOptions o)

      probabilityBox <- UI.div
      actionBox      <- preBlock
      classicalBox   <- preBlock
      quantumBox     <- preBlock
      finalBox       <- preBlock

      controls <- UI.div #+
        [ string "Program: "
        , element programSelect
        , UI.br
        , string "Outcome: "
        , element outcomeSelect
        , UI.br
        , string "Step: "
        , element stepSelect
        , element prevStepButton
        , element nextStepButton
        ]
        # set UI.style
            [ ("line-height", "2")
            , ("margin-bottom", "20px")
            ]

      viewer <- UI.div #+
        [ sectionTitle "Outcome probability"
        , element probabilityBox
        , sectionTitle "Action"
        , element actionBox
        , sectionTitle "Classical state"
        , element classicalBox
        , sectionTitle "Quantum state"
        , element quantumBox
        , sectionTitle "Final state of selected outcome"
        , element finalBox
        ]

      container <- UI.div #+
        [ element title
        , element controls
        , element viewer
        ]
        # set UI.style
            [ ("max-width", "1000px")
            , ("margin", "30px auto")
            , ("font-family", "sans-serif")
            ]

      getBody window #+ [element container]

      let renderSelected :: UI ()
          renderSelected = do
            progName <- get value programSelect
            outcomeIdStr <- get value outcomeSelect
            stepStr <- get value stepSelect

            let collection = findProgram progName collections
                outId = safeReadInt outcomeIdStr
                outcome = findOutcome outId collection
                stepNo = safeReadInt stepStr
                (finalClassical, finalQuantum) = prettyMem (finalState outcome)

            element probabilityBox # set text (prettyProbability (probability outcome))

            element finalBox # set text
              ( "Classical: " ++ finalClassical ++ "\n"
             ++ "Quantum: " ++ finalQuantum
              )

            if stepNo == 0
              then do
                let (cl, q) = prettyLMem (initialState collection)
                element actionBox    # set text "Initial state"
                element classicalBox # set text cl
                element quantumBox   # set text q
                return ()
              else do
                case findStep stepNo outcome of
                  Nothing -> do
                    element actionBox    # set text "Step not found."
                    element classicalBox # set text ""
                    element quantumBox   # set text ""
                    return ()

                  Just stp -> do
                    let (cl, q) = prettyMem (stateAfter stp)
                    element actionBox    # set text (action stp)
                    element classicalBox # set text cl
                    element quantumBox   # set text q
                    return ()

          changeStepBy :: Int -> UI ()
          changeStepBy delta = do
            progName <- get value programSelect
            outcomeIdStr <- get value outcomeSelect
            stepStr <- get value stepSelect

            let collection = findProgram progName collections
                outId = safeReadInt outcomeIdStr
                outcome = findOutcome outId collection
                currentStep = safeReadInt stepStr
                maxStep =
                  case steps outcome of
                    [] -> 0
                    xs -> maximum (map stepNumber xs)
                newStep = clamp 0 maxStep (currentStep + delta)

            element stepSelect # set value (show newStep)
            renderSelected

      on UI.selectionChange programSelect $ \_ -> do
        progName <- get value programSelect
        let collection = findProgram progName collections

        setOptions outcomeSelect (outcomeOptions collection)

        case outcomes collection of
          []    -> setOptions stepSelect []
          (o:_) -> setOptions stepSelect (stepOptions o)

        renderSelected

      on UI.selectionChange outcomeSelect $ \_ -> do
        progName <- get value programSelect
        outcomeIdStr <- get value outcomeSelect

        let collection = findProgram progName collections
            outId = safeReadInt outcomeIdStr
            outcome = findOutcome outId collection

        setOptions stepSelect (stepOptions outcome)
        renderSelected

      on UI.selectionChange stepSelect $ \_ ->
        renderSelected

      on UI.click prevStepButton $ \_ ->
        changeStepBy (-1)

      on UI.click nextStepButton $ \_ ->
        changeStepBy 1

      renderSelected
