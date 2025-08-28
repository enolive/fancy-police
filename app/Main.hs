{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Config (Config (..), getConfigDir, loadConfig)
import Data.Char (ord)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Yaml (FromJSON)
import GHC.Generics (Generic)
import Reporter (formatReport)
import Scanner (scanText)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import Text.Printf (printf)
import Types
import System.FilePath ((</>))

newtype Config = Config
  { thresholds :: Thresholds
  }
  deriving (Generic, Show, FromJSON)

main :: IO ()
main = do
  args <- getArgs
  let isPedantic = "--pedantic" `elem` args
  configDir <- getConfigDir
  config <- loadConfig $ configDir </> "fancy-police.yaml"
  input <- TIO.getContents
  let hits = scanText input
      (report, shouldFail) = formatReport hits (T.length input) isPedantic config.thresholds
  TIO.putStr report
  if shouldFail then exitFailure else exitSuccess

-- just a helper to give me the code point of a char in a usable version I can input into may code
debugChar :: Char -> IO ()
debugChar c = printf "Char '%c' has codepoint \\x%04X (%d)\n" c (ord c) (ord c)
