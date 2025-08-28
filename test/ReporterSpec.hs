{-# LANGUAGE OverloadedStrings #-}

module ReporterSpec (spec) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Reporter
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory)
import Test.Hspec
import Types

spec :: Spec
spec = do
  describe "Reporter for unicode shenanigans" $ do
    let defaultThresholds = Thresholds 10 0.3
        sampleGlyphHit = GlyphHit 1 3 $ Offender "MATHEMATICAL SANS-SERIF BOLD SMALL" "r" '\120319'

    it "prints report for heavy offender" $ do
      let hits =
            [ GlyphHit 1 1 $ Offender "MATHEMATICAL SANS-SERIF BOLD CAPITAL" "V" '\120297',
              GlyphHit 1 2 $ Offender "MATHEMATICAL SANS-SERIF BOLD SMALL" "e" '\120306',
              GlyphHit 1 3 $ Offender "MATHEMATICAL SANS-SERIF BOLD SMALL" "r" '\120319',
              GlyphHit 1 4 $ Offender "MATHEMATICAL SANS-SERIF BOLD SMALL" "t" '\120321',
              EmojiHit 7 1 "\128313",
              GlyphHit 7 3 $ Offender "MATHEMATICAL SANS-SERIF BOLD CAPITAL" "Z" '\120301',
              GlyphHit 7 4 $ Offender "MATHEMATICAL SANS-SERIF BOLD SMALL" "e" '\120306',
              GlyphHit 7 5 $ Offender "MATHEMATICAL SANS-SERIF BOLD SMALL" "n" '\120315',
              GlyphHit 7 6 $ Offender "MATHEMATICAL SANS-SERIF BOLD SMALL" "t" '\120321',
              GlyphHit 7 7 $ Offender "MATHEMATICAL SANS-SERIF BOLD SMALL" "r" '\120319',
              GlyphHit 7 8 $ Offender "MATHEMATICAL SANS-SERIF BOLD SMALL" "a" '\120302',
              GlyphHit 7 9 $ Offender "MATHEMATICAL SANS-SERIF BOLD SMALL" "l" '\120313',
              GlyphHit 7 62 $ Offender "EN DASH" "-" '\8211',
              EmojiHit 8 1 "\128313",
              GlyphHit 8 3 $ Offender "MATHEMATICAL SANS-SERIF BOLD CAPITAL" "S" '\120294',
              GlyphHit 8 4 $ Offender "MATHEMATICAL SANS-SERIF BOLD SMALL" "k" '\120312',
              GlyphHit 8 5 $ Offender "MATHEMATICAL SANS-SERIF BOLD SMALL" "a" '\120302',
              GlyphHit 8 6 $ Offender "MATHEMATICAL SANS-SERIF BOLD SMALL" "l" '\120313',
              GlyphHit 8 23 $ Offender "MATHEMATICAL SANS-SERIF BOLD SMALL" "f" '\120307',
              GlyphHit 8 70 $ Offender "EN DASH" "-" '\8211',
              EmojiHit 9 1 "\128313"
            ]
          totalChars = 40
          mode = Normal

      let (report, shouldFail) = formatReport hits totalChars mode defaultThresholds

      shouldFail `shouldBe` True
      goldenTest "ReporterSpec/heavy-offender" report

    it "prints report for a no-offender" $ do
      let hits = []
          totalChars = 40
          mode = Normal

      let (report, shouldFail) = formatReport hits totalChars mode defaultThresholds

      shouldFail `shouldBe` False
      goldenTest "ReporterSpec/no-offender" report

    it "prints report for a light offender in normal mode" $ do
      let hits = [sampleGlyphHit]
          totalChars = 40
          mode = Normal

      let (report, shouldFail) = formatReport hits totalChars mode defaultThresholds

      shouldFail `shouldBe` True
      goldenTest "ReporterSpec/light-offender" report

    it "prints report for a light offender in pedantic mode" $ do
      let hits = [sampleGlyphHit]
          totalChars = 40
          mode = Pedantic

      let (report, shouldFail) = formatReport hits totalChars mode defaultThresholds

      shouldFail `shouldBe` True
      goldenTest "ReporterSpec/light-offender-pedantic" report

    it "shows details when absolute threshold is exceeded" $ do
      let hits = replicate 3 sampleGlyphHit
          thresholds = Thresholds 3 0.5
          totalChars = 100
          mode = Normal

      let (report, shouldFail) = formatReport hits totalChars mode thresholds

      shouldFail `shouldBe` True
      goldenTest "ReporterSpec/absolute-threshold-exceeded" report

    it "hides details when absolute threshold is still fine" $ do
      let hits = replicate 2 sampleGlyphHit
          thresholds = Thresholds 3 0.5
          totalChars = 100
          mode = Normal

      let (report, shouldFail) = formatReport hits totalChars mode thresholds

      shouldFail `shouldBe` True
      goldenTest "ReporterSpec/absolute-threshold-fine" report

    it "provides brief summary without details even when threshold exceeded" $ do
      let hits = replicate 3 sampleGlyphHit
          totalChars = 100
          thresholds = Thresholds 2 0.01 -- Low thresholds that will be exceeded
          mode = Brief

      let (report, _) = formatReport hits totalChars mode thresholds

      goldenTest "ReporterSpec/brief-mode-threshold-exceeded" report

    it "shows details when density is exceeded" $ do
      let hits = replicate 2 sampleGlyphHit
          thresholds = Thresholds 999 0.5
          totalChars = 4
          mode = Normal

      let (report, shouldFail) = formatReport hits totalChars mode thresholds

      shouldFail `shouldBe` True
      goldenTest "ReporterSpec/density-threshold-exceeded" report

    it "hides details when density is still fine" $ do
      let hits = replicate 2 sampleGlyphHit
          totalChars = 5
          thresholds = Thresholds 999 0.5
          mode = Normal

      let (report, shouldFail) = formatReport hits totalChars mode thresholds

      shouldFail `shouldBe` True
      goldenTest "ReporterSpec/density-threshold-fine" report

    it "shows warnings but no details in brief mode when density exceeded" $ do
      let hits = replicate 2 sampleGlyphHit
          totalChars = 4 -- 2 Unicode chars out of 4 = 50% density
          thresholds = Thresholds 999 0.3 -- Density threshold of 30% will be exceeded
          mode = Brief

      let (report, _) = formatReport hits totalChars mode thresholds

      goldenTest "ReporterSpec/brief-mode-density-warning" report

    it "calculates 100% density for one-character emoji in single character text" $ do
      let emojiHit = EmojiHit 1 1 "🚀"
          hits = [emojiHit]
          totalChars = 1
          thresholds = Thresholds 999 0.5
          mode = Brief

      let (report, _) = formatReport hits totalChars mode thresholds

      goldenTest "ReporterSpec/density-for-simple-emojis" report

    it "calculates 100% density for complex emoji with multiple code units" $ do
      let complexEmoji = "\x1F9D1\x1F3FF\x200D\x1F680" -- 🧑🏿‍🚀 (person + dark skin + ZWJ + rocket)
          emojiHit = EmojiHit 1 1 complexEmoji
          hits = [emojiHit]
          totalChars = 4 -- The emoji consists of 4 code units
          thresholds = Thresholds 999 0.5
          mode = Brief

      let (report, _) = formatReport hits totalChars mode thresholds

      goldenTest "ReporterSpec/density-for-complex-emojis" report

goldenTest :: String -> T.Text -> IO ()
goldenTest testName actual = do
  let baseName = "test/golden/" ++ testName
  let goldenFile = baseName ++ ".golden"
  let actualFile = baseName ++ ".actual"

  createDirectoryIfMissing True (takeDirectory goldenFile)
  TIO.writeFile actualFile actual

  exists <- doesFileExist goldenFile
  if exists
    then do
      expected <- TIO.readFile goldenFile
      actual `shouldBe` expected
    else do
      TIO.writeFile goldenFile actual