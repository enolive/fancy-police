{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Config
  ( getConfigDir,
    loadConfig,
    Config(..),
    ConfigLoadError(..),
  )
where

import Control.Exception (throwIO, Exception)
import Data.Yaml (FromJSON, decodeFileEither, ParseException)
import GHC.Generics (Generic)
import System.Directory (getHomeDirectory)
import System.Environment (lookupEnv)
import Types
import System.FilePath ((</>))

-- | Configuration data structure
newtype Config = Config
  { thresholds :: Thresholds
  }
  deriving (Generic, Show, FromJSON)

data ConfigLoadError = ConfigLoadError {
  configPath :: FilePath,
  parseError :: ParseException
} deriving (Show, Exception)

-- | Get the configuration directory following XDG Base Directory Specification
getConfigDir :: IO FilePath
getConfigDir = do
  xdgConfig <- lookupEnv "XDG_CONFIG_HOME"
  case xdgConfig of
    Just dir -> return dir
    Nothing -> do
      homeDir <- getHomeDirectory
      return $ homeDir </> ".config"

-- | Load configuration from a YAML file
loadConfig :: FilePath -> IO Config
loadConfig configPath' = do
  result <- decodeFileEither configPath'
  case result of
    Left err -> throwIO ConfigLoadError { configPath = configPath', parseError = err }
    Right config -> return config