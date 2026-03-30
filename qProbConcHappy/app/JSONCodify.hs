{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}

module JSONCodify where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Aeson as Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as BL
import Data.Complex
import Data.Matrix (Matrix)
import qualified Data.Matrix as M
import System.IO
import System.Directory
import System.FilePath
import Control.Monad (unless)

import Syntax
import Collect_Samples


testJSONEncoding :: SampleCollection -> IO()
testJSONEncoding (name, l, samples) = do
  let json = encodeSampleCollection (name, l, samples)
  BL.putStr json

--------------------------------------------------------------------------------
-- Auxiliary JSON representations
--------------------------------------------------------------------------------

-- Complex numbers are encoded as:
-- { "re": ..., "im": ... }
complexToJSON :: Complex Double -> Value
complexToJSON (r :+ i) =
  object [ "re" .= r
         , "im" .= i
         ]

complexFromJSON :: Value -> Parser (Complex Double)
complexFromJSON = withObject "Complex Double" $ \o -> do
  r <- o .: "re"
  i <- o .: "im"
  pure (r :+ i)

-- Matrices are encoded as a list of rows,
-- each row being a list of complex numbers.
matrixToJSON :: Matrix (Complex Double) -> Value
matrixToJSON m =
  toJSON
    [ [ complexToJSON (M.getElem i j m)
      | j <- [1 .. M.ncols m]
      ]
    | i <- [1 .. M.nrows m]
    ]

matrixFromJSON :: Value -> Parser (Matrix (Complex Double))
matrixFromJSON v = do
  rowsAsValues <- parseJSON v :: Parser [[Value]]
  rows <- traverse (traverse complexFromJSON) rowsAsValues
  pure (M.fromLists rows)

--------------------------------------------------------------------------------
-- Encoder
--------------------------------------------------------------------------------

encodeSampleCollection :: SampleCollection -> BL.ByteString
encodeSampleCollection (programName, l, samples) =
  Aeson.encode $
    object
      [ "1-programName" .= programName,
        "2-linkingFunction" .= l,
        "3-samples" .= map encodeSample samples
      ]
  where
    encodeSample :: (Int, (StC, StQ)) -> Value
    encodeSample (n, (stc, stq)) =
      object
        [ "sampleId" .= n,
          "stc" .= stc,
          "stq" .= matrixToJSON stq
        ]

--------------------------------------------------------------------------------
-- Decoder
--------------------------------------------------------------------------------

decodeSampleCollection :: BL.ByteString -> Either String SampleCollection
decodeSampleCollection bs =
  eitherDecode bs >>= parseEither parseSampleCollectionValue

parseSampleCollectionValue :: Value -> Parser SampleCollection
parseSampleCollectionValue =
  withObject "SampleCollection" $ \o -> do
    programName <- o .: "1-programName"
    l <- o .: "2-linkingFunction"
    sampleVals  <- o .: "3-samples"
    samples     <- traverse parseSample sampleVals
    pure (programName, l, samples)
  where
    parseSample :: Value -> Parser (Int, (StC, StQ))
    parseSample =
      withObject "sample entry" $ \o -> do
        n   <- o .: "sampleId"
        stc <- o .: "stc"
        stqVal <- o .: "stq"
        stq <- matrixFromJSON stqVal
        pure (n, (stc, stq))


prepareJsonFile :: FilePath -> IO (FilePath)
prepareJsonFile inputPath = do
  -- 1) Break path by "/"
  let parts = splitDirectories inputPath

  -- Reconstruct directory and filename
  let dir      = joinPath (init parts)     -- directory of the file
  let fileName = last parts                -- e.g. "file_name.txt"

  -- 2) Create "json" directory inside the same directory
  let jsonDir = dir </> "json"
  createDirectoryIfMissing True jsonDir

  -- 3) Create json file with same base name
  let baseName = dropExtension fileName
  let jsonFile = jsonDir </> (baseName <.> "json")

  exists <- doesFileExist jsonFile
  unless exists $ writeFile jsonFile ""   -- create empty file

  putStrLn $ "JSON file ready at: " ++ jsonFile

  return $ jsonFile

resetJsonFile :: FilePath -> IO ()
resetJsonFile path =
  writeFile path ""   -- clears file

appendJson :: FilePath -> SampleCollection -> IO ()
appendJson path value = withFile path AppendMode $ \h -> do
  BL.hPutStr h (encodeSampleCollection value)
  hPutStr h "\n"  
