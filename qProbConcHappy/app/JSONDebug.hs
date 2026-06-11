{-# LANGUAGE OverloadedStrings #-}

module JSONDebug
  ( JMem(..)
  , JLMem(..)
  , DebugStepJSON(..)
  , DebugOutcomeJSON(..)
  , DebugCollectionJSON(..)
  , encodeDebugCollection
  , decodeDebugCollection
  , debugResultToJSON
  , appendDebugJson
  , prepareDebugJsonFile
  , resetDebugJsonFile
  ) where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Aeson as Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Matrix (Matrix)
import System.Directory
import System.FilePath
import System.IO
import Control.Monad (unless)

import Syntax
import Debug (Hist, showStQ)
import Beautify (showStC, limitPrecisionS)
import JSONCodify (matrixToJSON, matrixFromJSON)

--------------------------------------------------------------------------------
-- JSON-friendly representations for debug data
--------------------------------------------------------------------------------

data JMem = JMem
  { jStc       :: StC
  , jStq       :: StQ
  , jClassical :: String
  , jQuantum   :: String
  } deriving (Show, Eq)

data JLMem = JLMem
  { jlStc       :: StC
  , jlLink      :: L
  , jlStq       :: StQ
  , jlClassical :: String
  , jlQuantum   :: String
  } deriving (Show, Eq)

data DebugStepJSON = DebugStepJSON
  { stepNumber :: Int
  , action     :: String
  , stateAfter :: JLMem
  } deriving (Show, Eq)

data DebugOutcomeJSON = DebugOutcomeJSON
  { outcomeId   :: Int
  , probability :: Double
  , finalState  :: JMem
  , steps       :: [DebugStepJSON]
  } deriving (Show, Eq)

data DebugCollectionJSON = DebugCollectionJSON
  { programName  :: String
  , initialState :: JLMem
  , outcomes     :: [DebugOutcomeJSON]
  } deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Encoding
--------------------------------------------------------------------------------

prettyObject :: String -> String -> Value
prettyObject classical quantum =
  object
    [ "classical" .= classical
    , "quantum"   .= quantum
    ]

instance ToJSON JMem where
  toJSON (JMem stc stq classical quantum) =
    object
      [ "stc"    .= stc
      , "stq"    .= matrixToJSON stq
      , "pretty" .= prettyObject classical quantum
      ]

instance ToJSON JLMem where
  toJSON (JLMem stc link stq classical quantum) =
    object
      [ "stc"             .= stc
      , "linkingFunction" .= link
      , "stq"             .= matrixToJSON stq
      , "pretty"          .= prettyObject classical quantum
      ]

instance ToJSON DebugStepJSON where
  toJSON (DebugStepJSON n act st) =
    object
      [ "step"       .= n
      , "action"     .= act
      , "stateAfter" .= st
      ]

instance ToJSON DebugOutcomeJSON where
  toJSON (DebugOutcomeJSON oid prob final steps_) =
    object
      [ "outcomeId"   .= oid
      , "probability" .= prob
      , "final"       .= final
      , "steps"       .= steps_
      ]

instance ToJSON DebugCollectionJSON where
  toJSON (DebugCollectionJSON name initSt outs) =
    object
      [ "programName" .= name
      , "initial"     .= initSt
      , "outcomes"    .= outs
      ]

--------------------------------------------------------------------------------
-- Decoding
--------------------------------------------------------------------------------

parsePretty :: Object -> Parser (String, String)
parsePretty o = do
  pretty <- o .: "pretty"
  flip (withObject "pretty") pretty $ \p -> do
    classical <- p .: "classical"
    quantum   <- p .: "quantum"
    pure (classical, quantum)

instance FromJSON JMem where
  parseJSON = withObject "JMem" $ \o -> do
    stc <- o .: "stc"
    stqVal <- o .: "stq"
    stq <- matrixFromJSON stqVal
    (classical, quantum) <- parsePretty o
    pure $ JMem stc stq classical quantum

instance FromJSON JLMem where
  parseJSON = withObject "JLMem" $ \o -> do
    stc <- o .: "stc"
    link <- o .: "linkingFunction"
    stqVal <- o .: "stq"
    stq <- matrixFromJSON stqVal
    (classical, quantum) <- parsePretty o
    pure $ JLMem stc link stq classical quantum

instance FromJSON DebugStepJSON where
  parseJSON = withObject "DebugStepJSON" $ \o ->
    DebugStepJSON
      <$> o .: "step"
      <*> o .: "action"
      <*> o .: "stateAfter"

instance FromJSON DebugOutcomeJSON where
  parseJSON = withObject "DebugOutcomeJSON" $ \o ->
    DebugOutcomeJSON
      <$> o .: "outcomeId"
      <*> o .: "probability"
      <*> o .: "final"
      <*> o .: "steps"

instance FromJSON DebugCollectionJSON where
  parseJSON = withObject "DebugCollectionJSON" $ \o ->
    DebugCollectionJSON
      <$> o .: "programName"
      <*> o .: "initial"
      <*> o .: "outcomes"

--------------------------------------------------------------------------------
-- Conversion from Debug.run_debug_KStepSch output
--------------------------------------------------------------------------------

memToJMem :: Mem -> JMem
memToJMem (stc, stq) =
  let stq' = limitPrecisionS 5 stq
  in JMem
       { jStc       = stc
       , jStq       = stq'
       , jClassical = showStC stc
       , jQuantum   = showStQ stq'
       }

lmemToJLMem :: LMem -> JLMem
lmemToJLMem (stc, link, stq) =
  let stq' = limitPrecisionS 5 stq
  in JLMem
       { jlStc       = stc
       , jlLink      = link
       , jlStq       = stq'
       , jlClassical = showStC stc
       , jlQuantum   = showStQ stq'
       }

histToSteps :: Hist -> [DebugStepJSON]
histToSteps hist = zipWith encodeStep [1 :: Int ..] hist
  where
    encodeStep n (act, lmem) =
      DebugStepJSON
        { stepNumber = n
        , action     = act
        , stateAfter = lmemToJLMem lmem
        }

-- | Convert one program debug result into the JSON structure consumed by GuiDebug.
--
-- The input result is exactly the value returned by:
--   run_debug_KStepSch sch_d c initial k
-- whose type is:
--   [((Mem, Hist), Double)]
debugResultToJSON :: String -> LMem -> [((Mem, Hist), Double)] -> DebugCollectionJSON
debugResultToJSON name initial result =
  DebugCollectionJSON
    { programName  = name
    , initialState = lmemToJLMem initial
    , outcomes     = zipWith encodeOutcome [1 :: Int ..] result
    }
  where
    encodeOutcome oid ((finalMem, hist), prob) =
      DebugOutcomeJSON
        { outcomeId   = oid
        , probability = prob
        , finalState  = memToJMem finalMem
        , steps       = histToSteps hist
        }

--------------------------------------------------------------------------------
-- File helpers
--------------------------------------------------------------------------------

encodeDebugCollection :: DebugCollectionJSON -> BL.ByteString
encodeDebugCollection = encodePretty

decodeDebugCollection :: BL.ByteString -> Either String DebugCollectionJSON
decodeDebugCollection = Aeson.eitherDecode

--  | Creates json/<input-base>_debug.json next to the input file.
--  This avoids clashing with the histogram JSON file created by JSONCodify.prepareJsonFile.
prepareDebugJsonFile :: FilePath -> IO FilePath
prepareDebugJsonFile inputPath = do
  let parts = splitDirectories inputPath
      dir = joinPath (init parts)
      fileName = last parts
      jsonDir = dir </> "json"
      baseName = dropExtension fileName
      jsonFile = jsonDir </> (baseName ++ "_debug" <.> "json")

  createDirectoryIfMissing True jsonDir
  exists <- doesFileExist jsonFile
  unless exists $ BL.writeFile jsonFile (encodePretty ([] :: [DebugCollectionJSON]))

  putStrLn $ "Debug JSON file ready at: " ++ jsonFile
  pure jsonFile

resetDebugJsonFile :: FilePath -> IO ()
resetDebugJsonFile path =
  BL.writeFile path (encodePretty ([] :: [DebugCollectionJSON]))

appendDebugJson :: FilePath -> DebugCollectionJSON -> IO ()
appendDebugJson path value = do
  content <- BS.readFile path

  let oldValues =
        case Aeson.eitherDecodeStrict' content :: Either String [DebugCollectionJSON] of
          Right xs -> xs
          Left _   -> []

      newValues = oldValues ++ [value]

  BL.writeFile path (encodePretty newValues)
