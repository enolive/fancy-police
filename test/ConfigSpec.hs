{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ConfigSpec where

import Config
import Control.Exception (bracket_)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Types

spec :: Spec
spec = describe "Configuration handling" $ do
  describe "XDG config directory resolution" $ do
    it "uses XDG_CONFIG_HOME when set" $ do
      result <- withEnvVar "XDG_CONFIG_HOME" "/custom/config" getConfigDir

      result `shouldBe` "/custom/config"

    it "falls back to ~/.config when XDG_CONFIG_HOME is unset" $ do
      result <- withoutEnvVar "XDG_CONFIG_HOME" $ withEnvVar "HOME" "/home/user" getConfigDir

      result `shouldBe` "/home/user/.config"

  describe "Configuration file loading" $ do
    it "parses valid YAML configuration" $ do
      withSystemTempDirectory "config-test" $ \tmpDir -> do
        let configFile = tmpDir </> "test-config.yaml"
        writeFile configFile $
          unlines
            [ "thresholds:",
              "  absolute: 10",
              "  density: 5.0"
            ]

        config <- loadConfig configFile

        config.thresholds.absolute `shouldBe` 10
        config.thresholds.density `shouldBe` 5.0

    it "rejects invalid YAML syntax" $ do
      withSystemTempDirectory "config-test" $ \tmpDir -> do
        let configFile = tmpDir </> "bad-config.yaml"
        writeFile configFile "invalid: yaml: syntax: ["

        loadConfig configFile `shouldThrow` \case
          ConfigLoadError path _ -> path == configFile

-- | Test helper for safely setting/unsetting environment variables
withEnvVar :: String -> String -> IO a -> IO a
withEnvVar key value action = do
  originalValue <- lookupEnv key
  bracket_
    (setEnv key value)
    (maybe (unsetEnv key) (setEnv key) originalValue)
    action

-- | Test helper for temporarily unsetting an environment variable
withoutEnvVar :: String -> IO a -> IO a
withoutEnvVar key action = do
  originalValue <- lookupEnv key
  bracket_
    (unsetEnv key)
    (maybe (unsetEnv key) (setEnv key) originalValue)
    action
