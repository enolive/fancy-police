{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Char (ord)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Yaml (FromJSON, decodeFileEither)
import GHC.Generics (Generic)
import Reporter (formatReport)
import Scanner (scanText)
import System.Directory (getHomeDirectory)
import System.Environment (getArgs, lookupEnv)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>))
import Text.Printf (printf)
import Types

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

getConfigDir :: IO FilePath
getConfigDir = do
  xdgConfig <- lookupEnv "XDG_CONFIG_HOME"
  case xdgConfig of
    Just dir -> return dir
    Nothing -> do
      homeDir <- getHomeDirectory
      return $ homeDir </> ".config"

loadConfig :: FilePath -> IO Config
loadConfig path = do
  result <- decodeFileEither path
  case result of
    Left err -> error $ "Error loading config: " ++ show err
    Right cfg -> return cfg

-- just a helper to give me the code point of a char in a usable version I can input into may code
debugChar :: Char -> IO ()
debugChar c = printf "Char '%c' has codepoint \\x%04X (%d)\n" c (ord c) (ord c)
