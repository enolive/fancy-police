{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Reporter (formatReport) where

import Data.Char (ord)
import qualified Data.Text as T
import Text.Printf (printf)
import Types

type ShouldFail = Bool

formatReport :: [Hit] -> Int -> Bool -> Thresholds -> (T.Text, ShouldFail)
formatReport [] _ _ _ =
  ("FancyPolice: No Unicode gremlins found. Carry on, ASCII astronaut. \x1F9D1\x200D\x1F680\n", False)
formatReport hits totalChars isPedantic thresholds =
  let totalHits = length hits
      countedChars = countHitChars hits
      calculatedDensity = calculateDensity countedChars totalChars
      shouldShowDetails = isPedantic || totalHits >= thresholds.absolute || calculatedDensity >= thresholds.density
      report =
        T.unlines $
          summary totalHits calculatedDensity
            ++ warning totalHits calculatedDensity thresholds
            ++ details hits shouldShowDetails
   in (report, True)

summary :: Int -> Double -> [T.Text]
summary totalHits totalDensity =
  [ "FancyPolice: Unicode shenanigans detected! 🕵️",
    T.pack $ printf "Found %d suspicious characters (%.2f%% density)" totalHits (totalDensity * 100)
  ]

warning :: Int -> Double -> Thresholds -> [T.Text]
warning totalHits density' thresholds =
  let absoluteWarning =
        ([T.pack $ printf "🚨  HIGH OFFENSE COUNT: %d violations detected!" totalHits | totalHits >= thresholds.absolute])
      densityWarning =
        (["🚨  HIGH DENSITY: Too much Unicode glitter detected!" | density' >= thresholds.density])
   in absoluteWarning ++ densityWarning ++ (["" | not (null absoluteWarning && null densityWarning)])

details :: [Hit] -> Bool -> [T.Text]
details hits True = "Detailed violations:" : map formatHit hits
details _ False =
  [ "💡 Tip: Use --pedantic flag to see detailed violations",
    "🎯 Or fix the major issues first (threshold exceeded = detailed report)"
  ]

formatHit :: Hit -> T.Text
formatHit hit@GlyphHit {} =
  T.pack $
    printf
      "- line %d, col %d: found \"%c\" (%s) [U+%04X] -> replace with \"%s\""
      hit.lineNumber
      hit.colNumber
      hit.offender.culprit
      hit.offender.name
      (ord hit.offender.culprit)
      hit.offender.suggestion
formatHit hit@EmojiHit {} =
  T.pack $
    printf
      "- line %d, col %d: found \"%s\" (%s)"
      hit.lineNumber
      hit.colNumber
      hit.seqText
      (emojiWhy hit.seqText)

-- Why/fun message for emoji clusters
emojiWhy :: T.Text -> T.Text
emojiWhy cluster =
  let codePoints = T.pack . printf "U+%04X" . ord <$> T.unpack cluster
      uxs = T.intercalate ", " codePoints
   in T.pack $ printf "EMOJI sequence (%s) -> replace with ':emoji:' or ASCII, e.g., ':rocket:'." uxs

countHitChars :: [Hit] -> Int
countHitChars = sum . map countCharsForHit
  where
    countCharsForHit (GlyphHit {}) = 1
    countCharsForHit (EmojiHit _ _ emojiText) = T.length emojiText

calculateDensity :: Int -> Int -> Double
calculateDensity _ 0 = 0
calculateDensity unicodeChars totalChars = fromIntegral unicodeChars / fromIntegral totalChars