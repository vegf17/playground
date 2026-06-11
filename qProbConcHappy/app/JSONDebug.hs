{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

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
  , writeDebugJsonFile
  , prepareDebugJsonFile
  , resetDebugJsonFile
  ) where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Aeson as Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import System.Directory
import System.FilePath
import Control.Monad (unless)

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

import Syntax
import Debug (Hist, showStQ)
import Beautify (showStC, limitPrecisionS)

--------------------------------------------------------------------------------
-- JSON-friendly representations for debug data
--------------------------------------------------------------------------------

data JMem = JMem
  { jClassical :: String
  , jQuantum   :: String
  } deriving (Show, Eq, Generic, NFData)

data JLMem = JLMem
  { jlClassical :: String
  , jlQuantum   :: String
  , jlLink      :: L
  } deriving (Show, Eq, Generic, NFData)

data DebugStepJSON = DebugStepJSON
  { stepNumber :: Int
  , action     :: String
  , stateAfter :: JMem
  } deriving (Show, Eq, Generic, NFData)

data DebugOutcomeJSON = DebugOutcomeJSON
  { outcomeId   :: Int
  , probability :: Double
  , finalState  :: JMem
  , steps       :: [DebugStepJSON]
  } deriving (Show, Eq, Generic, NFData)

data DebugCollectionJSON = DebugCollectionJSON
  { programName  :: String
  , initialState :: JLMem
  , outcomes     :: [DebugOutcomeJSON]
  } deriving (Show, Eq, Generic, NFData)

--------------------------------------------------------------------------------
-- Encoding
--------------------------------------------------------------------------------

instance ToJSON JMem where
  toJSON (JMem classical quantum) =
    object
      [ "classical" .= classical
      , "quantum"   .= quantum
      ]

instance ToJSON JLMem where
  toJSON (JLMem classical quantum link) =
    object
      [ "classical" .= classical
      , "quantum"   .= quantum
      , "linkingFunction" .= link
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

instance FromJSON JMem where
  parseJSON = withObject "JMem" $ \o ->
    JMem
      <$> o .: "classical"
      <*> o .: "quantum"

instance FromJSON JLMem where
  parseJSON = withObject "JLMem" $ \o ->
    JLMem
      <$> o .: "classical"
      <*> o .: "quantum"
      <*> o .: "linkingFunction"

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
       { jClassical = showStC stc
       , jQuantum   = "<hidden>" --showStQ stq'
       }

lmemToJLMem :: LMem -> JLMem
lmemToJLMem (stc, link, stq) =
  let stq' = limitPrecisionS 5 stq
  in JLMem
       { jlClassical = showStC stc
       , jlQuantum   = "<hidden>" --showStQ stq'
       , jlLink      = link
       }

histToSteps :: Hist -> [DebugStepJSON]
histToSteps hist = zipWith encodeStep [1 :: Int ..] hist
  where
    encodeStep n (act, (stc, _link, stq)) =
      DebugStepJSON
        { stepNumber = n
        , action     = act
        , stateAfter = memToJMem (stc, stq)
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

-- Pretty encoding of one debug collection.
-- Useful for testing, but the file should normally contain [DebugCollectionJSON].
encodeDebugCollection :: DebugCollectionJSON -> BL.ByteString
encodeDebugCollection = encodePretty

decodeDebugCollection :: BL.ByteString -> Either String DebugCollectionJSON
decodeDebugCollection = Aeson.eitherDecode

-- | Creates json/<input-base>_debug.json next to the input file.
-- This avoids clashing with the histogram JSON file created by JSONCodify.prepareJsonFile.
prepareDebugJsonFile :: FilePath -> IO FilePath
prepareDebugJsonFile inputPath = do
  let parts = splitDirectories inputPath
      dir =
        case init parts of
          [] -> "."
          xs -> joinPath xs
      fileName = last parts
      jsonDir = dir </> "json"
      baseName = dropExtension fileName
      jsonFile = jsonDir </> (baseName ++ "_debug" <.> "json")

  createDirectoryIfMissing True jsonDir

  exists <- doesFileExist jsonFile
  unless exists $
    BL.writeFile jsonFile (encodePretty ([] :: [DebugCollectionJSON]))

  putStrLn $ "Debug JSON file ready at: " ++ jsonFile
  pure jsonFile

resetDebugJsonFile :: FilePath -> IO ()
resetDebugJsonFile path =
  BL.writeFile path (encodePretty ([] :: [DebugCollectionJSON]))

-- Fast version.
-- Use this whenever you already have all DebugCollectionJSON values.
-- It writes the file once.
writeDebugJsonFilePretty :: FilePath -> [DebugCollectionJSON] -> IO ()
writeDebugJsonFilePretty path values =
  BL.writeFile path (encodePretty values)

--compact version, which prints all the JSON in one line
writeDebugJsonFile :: FilePath -> [DebugCollectionJSON] -> IO ()
writeDebugJsonFile path values =
  BL.writeFile path (Aeson.encode values)

-- Slower compatibility version.
-- This reads the whole file, appends one value, and rewrites the whole file.
-- Prefer writeDebugJsonFile when possible.
appendDebugJson :: FilePath -> DebugCollectionJSON -> IO ()
appendDebugJson path value = do
  content <- BS.readFile path

  let oldValues =
        case Aeson.eitherDecodeStrict' content :: Either String [DebugCollectionJSON] of
          Right xs -> xs
          Left _   -> []

      newValues = oldValues ++ [value]

  BL.writeFile path (encodePretty newValues)
