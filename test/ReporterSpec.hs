{-# LANGUAGE OverloadedStrings #-}

module ReporterSpec (spec) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Reporter (formatReport)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory)
import Test.Hspec
import Types

spec :: Spec
spec = do
  describe "Reporter for unicode shenanigans" $ do
    let defaultThresholds = Thresholds {absolute = 10, density = 0.3}

    it "prints report for heavy offender" $ do
      let heavyOffender =
            [ GlyphHit {lineNumber = 1, colNumber = 1, offender = Offender {name = "MATHEMATICAL SANS-SERIF BOLD CAPITAL", suggestion = "V", culprit = '\120297'}},
              GlyphHit {lineNumber = 1, colNumber = 2, offender = Offender {name = "MATHEMATICAL SANS-SERIF BOLD SMALL", suggestion = "e", culprit = '\120306'}},
              GlyphHit {lineNumber = 1, colNumber = 3, offender = Offender {name = "MATHEMATICAL SANS-SERIF BOLD SMALL", suggestion = "r", culprit = '\120319'}},
              GlyphHit {lineNumber = 1, colNumber = 4, offender = Offender {name = "MATHEMATICAL SANS-SERIF BOLD SMALL", suggestion = "t", culprit = '\120321'}},
              EmojiHit {lineNumber = 7, colNumber = 1, seqText = "\128313"},
              GlyphHit {lineNumber = 7, colNumber = 3, offender = Offender {name = "MATHEMATICAL SANS-SERIF BOLD CAPITAL", suggestion = "Z", culprit = '\120301'}},
              GlyphHit {lineNumber = 7, colNumber = 4, offender = Offender {name = "MATHEMATICAL SANS-SERIF BOLD SMALL", suggestion = "e", culprit = '\120306'}},
              GlyphHit {lineNumber = 7, colNumber = 5, offender = Offender {name = "MATHEMATICAL SANS-SERIF BOLD SMALL", suggestion = "n", culprit = '\120315'}},
              GlyphHit {lineNumber = 7, colNumber = 6, offender = Offender {name = "MATHEMATICAL SANS-SERIF BOLD SMALL", suggestion = "t", culprit = '\120321'}},
              GlyphHit {lineNumber = 7, colNumber = 7, offender = Offender {name = "MATHEMATICAL SANS-SERIF BOLD SMALL", suggestion = "r", culprit = '\120319'}},
              GlyphHit {lineNumber = 7, colNumber = 8, offender = Offender {name = "MATHEMATICAL SANS-SERIF BOLD SMALL", suggestion = "a", culprit = '\120302'}},
              GlyphHit {lineNumber = 7, colNumber = 9, offender = Offender {name = "MATHEMATICAL SANS-SERIF BOLD SMALL", suggestion = "l", culprit = '\120313'}},
              GlyphHit {lineNumber = 7, colNumber = 62, offender = Offender {name = "EN DASH", suggestion = "-", culprit = '\8211'}},
              EmojiHit {lineNumber = 8, colNumber = 1, seqText = "\128313"},
              GlyphHit {lineNumber = 8, colNumber = 3, offender = Offender {name = "MATHEMATICAL SANS-SERIF BOLD CAPITAL", suggestion = "S", culprit = '\120294'}},
              GlyphHit {lineNumber = 8, colNumber = 4, offender = Offender {name = "MATHEMATICAL SANS-SERIF BOLD SMALL", suggestion = "k", culprit = '\120312'}},
              GlyphHit {lineNumber = 8, colNumber = 5, offender = Offender {name = "MATHEMATICAL SANS-SERIF BOLD SMALL", suggestion = "a", culprit = '\120302'}},
              GlyphHit {lineNumber = 8, colNumber = 6, offender = Offender {name = "MATHEMATICAL SANS-SERIF BOLD SMALL", suggestion = "l", culprit = '\120313'}},
              GlyphHit {lineNumber = 8, colNumber = 23, offender = Offender {name = "MATHEMATICAL SANS-SERIF BOLD SMALL", suggestion = "f", culprit = '\120307'}},
              GlyphHit {lineNumber = 8, colNumber = 70, offender = Offender {name = "EN DASH", suggestion = "-", culprit = '\8211'}},
              EmojiHit {lineNumber = 9, colNumber = 1, seqText = "\128313"}
            ]
      let (report, shouldFail) = formatReport heavyOffender 40 False defaultThresholds

      shouldFail `shouldBe` True
      goldenTest "ReporterSpec/heavy-offender" report

    it "prints report for a no-offender" $ do
      let hits = []

      let (report, shouldFail) = formatReport hits 40 False defaultThresholds

      shouldFail `shouldBe` False
      goldenTest "ReporterSpec/no-offender" report

    it "prints report for a light offender in not pedantic mode" $ do
      let hits =
            [ GlyphHit {lineNumber = 1, colNumber = 3, offender = Offender {name = "MATHEMATICAL SANS-SERIF BOLD SMALL", suggestion = "r", culprit = '\120319'}}
            ]

      let (report, shouldFail) = formatReport hits 40 False defaultThresholds

      shouldFail `shouldBe` True
      goldenTest "ReporterSpec/light-offender" report

    it "prints report for a light offender in pedantic mode" $ do
      let hits =
            [ GlyphHit {lineNumber = 1, colNumber = 3, offender = Offender {name = "MATHEMATICAL SANS-SERIF BOLD SMALL", suggestion = "r", culprit = '\120319'}}
            ]

      let (report, shouldFail) = formatReport hits 40 True defaultThresholds

      shouldFail `shouldBe` True
      goldenTest "ReporterSpec/light-offender-pedantic" report

    it "shows details when absolute threshold is exceeded" $ do
      let hits =
            [ GlyphHit {lineNumber = 1, colNumber = 3, offender = Offender {name = "MATHEMATICAL SANS-SERIF BOLD SMALL", suggestion = "r", culprit = '\120319'}},
              GlyphHit {lineNumber = 1, colNumber = 3, offender = Offender {name = "MATHEMATICAL SANS-SERIF BOLD SMALL", suggestion = "r", culprit = '\120319'}},
              GlyphHit {lineNumber = 1, colNumber = 3, offender = Offender {name = "MATHEMATICAL SANS-SERIF BOLD SMALL", suggestion = "r", culprit = '\120319'}}
            ]

      let (report, shouldFail) = formatReport hits 100 False defaultThresholds {absolute = 3}

      shouldFail `shouldBe` True
      goldenTest "ReporterSpec/absolute-threshold-exceeded" report

    it "hides details when absolute threshold is still fine" $ do
      let hits =
            [ GlyphHit {lineNumber = 1, colNumber = 3, offender = Offender {name = "MATHEMATICAL SANS-SERIF BOLD SMALL", suggestion = "r", culprit = '\120319'}},
              GlyphHit {lineNumber = 1, colNumber = 3, offender = Offender {name = "MATHEMATICAL SANS-SERIF BOLD SMALL", suggestion = "r", culprit = '\120319'}}
            ]

      let (report, shouldFail) = formatReport hits 100 False defaultThresholds {absolute = 3}

      shouldFail `shouldBe` True
      goldenTest "ReporterSpec/absolute-threshold-fine" report

    it "shows details when density is exceeded" $ do
      let hits =
            [ GlyphHit {lineNumber = 1, colNumber = 3, offender = Offender {name = "MATHEMATICAL SANS-SERIF BOLD SMALL", suggestion = "r", culprit = '\120319'}},
              GlyphHit {lineNumber = 1, colNumber = 3, offender = Offender {name = "MATHEMATICAL SANS-SERIF BOLD SMALL", suggestion = "r", culprit = '\120319'}}
            ]

      let (report, shouldFail) = formatReport hits 4 False defaultThresholds {density = 0.5}

      shouldFail `shouldBe` True
      goldenTest "ReporterSpec/density-threshold-exceeded" report

    it "hides details when density is still fine" $ do
      let hits =
            [ GlyphHit {lineNumber = 1, colNumber = 3, offender = Offender {name = "MATHEMATICAL SANS-SERIF BOLD SMALL", suggestion = "r", culprit = '\120319'}},
              GlyphHit {lineNumber = 1, colNumber = 3, offender = Offender {name = "MATHEMATICAL SANS-SERIF BOLD SMALL", suggestion = "r", culprit = '\120319'}}
            ]

      let (report, shouldFail) = formatReport hits 5 False defaultThresholds {density = 0.5}

      shouldFail `shouldBe` True
      goldenTest "ReporterSpec/density-threshold-fine" report

    it "calculates 100% density for one-character emoji in single character text" $ do
      let emojiHit = EmojiHit 1 1 "🚀"
          hits = [emojiHit]
          totalChars = 1
          thresholds = Thresholds 999 0.5
          isPedantic = True

      let (report, _) = formatReport hits totalChars isPedantic thresholds

      report `shouldSatisfy` \r -> "100.00% density" `T.isInfixOf` r

    it "calculates 100% density for complex emoji with multiple code units" $ do
      let complexEmoji = "\x1F9D1\x1F3FF\x200D\x1F680" -- 🧑🏿‍🚀 (person + dark skin + ZWJ + rocket)
          emojiHit = EmojiHit 1 1 complexEmoji
          hits = [emojiHit]
          totalChars = 4 -- The emoji consists of 4 code units
          thresholds = Thresholds 999 0.5
          isPedantic = True

      let (report, _) = formatReport hits totalChars isPedantic thresholds

      report `shouldSatisfy` \r -> "100.00% density" `T.isInfixOf` r

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