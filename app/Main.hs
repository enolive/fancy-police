{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (when)
import Data.Char (chr, ord)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import Text.Printf (printf)
import Unicode.Emoji (takeEmojiCluster)

-- Thresholds for reporting
data Thresholds = Thresholds
  { absolute :: Int,
    density :: Double
  }

thresholds :: Thresholds
thresholds = Thresholds {absolute = 10, density = 0.03}

data Offender = Offender
  { name :: T.Text,
    suggestion :: T.Text,
    culprit :: Char
  }
  deriving (Show)

-- Offensive unicode characters
gremlins :: M.Map Char Offender
gremlins =
  M.fromList $
    fmap
      (\(c, n, s) -> (c, Offender {name = n, suggestion = s, culprit = c}))
      [ ('\x2013', "EN DASH", "-"),
        ('\x2014', "EM DASH", "-"),
        ('\x2212', "MINUS SIGN", "-"),
        ('\x201C', "LEFT DOUBLE QUOTATION MARK", "\""),
        ('\x201D', "RIGHT DOUBLE QUOTATION MARK", "\""),
        ('\x201E', "DOUBLE LOW-9 QUOTATION MARK", "\""),
        ('\x2018', "LEFT SINGLE QUOTATION MARK", "'"),
        ('\x2019', "RIGHT SINGLE QUOTATION MARK", "'"),
        ('\x2026', "HORIZONTAL ELLIPSIS", "..."),
        ('\x00A0', "NO-BREAK SPACE", " "),
        ('\x2009', "THIN SPACE", " "),
        ('\x2002', "EN SPACE", " "),
        ('\x2003', "EM SPACE", " "),
        ('\x200B', "ZERO WIDTH SPACE", ""),
        ('\x200D', "ZERO WIDTH JOINER", ""),
        ('\x00D7', "MULTIPLICATION SIGN", "*"),
        ('\x2215', "DIVISION SLASH", "/"),
        ('\x2044', "FRACTION SLASH", "/"),
        ('\x2192', "RIGHTWARDS ARROW", "->"),
        ('\x21D2', "RIGHTWARDS DOUBLE ARROW", "=>"),
        ('\x2022', "BULLET", "*"),
        ('\x2122', "TRADE MARK SIGN", "(TM)"),
        ('\x00AE', "REGISTERED SIGN", "(R)"),
        ('\x00B8', "CEDILLA", ","),
        ('\x1E9E', "LATIN CAPITAL LETTER SHARP S", "SS"),
        -- Lookalikes
        ('\x00B5', "MICRO SIGN (not greek!)", "u"),
        -- Greek lookalikes
        ('\x0391', "GREEK ALPHA", "A"),
        ('\x0392', "GREEK BETA", "B"),
        ('\x0395', "GREEK EPSILON", "E"),
        ('\x0396', "GREEK ZETA", "Z"),
        ('\x0397', "GREEK ETA", "H"),
        ('\x0399', "GREEK IOTA", "I"),
        ('\x039A', "GREEK KAPPA", "K"),
        ('\x039C', "GREEK MU", "M"),
        ('\x039D', "GREEK NU", "N"),
        ('\x039F', "GREEK OMICRON", "O"),
        ('\x03A1', "GREEK RHO", "P"),
        ('\x03A4', "GREEK TAU", "T"),
        ('\x03A5', "GREEK UPSILON", "Y"),
        ('\x03A7', "GREEK CHI", "X"),
        ('\x03BC', "GREEK SMALL MU", "u"),
        ('\x037E', "GREEK QUESTION MARK", ";"),
        -- Cyrillic lookalikes
        ('\x0410', "CYRILLIC A", "A"),
        ('\x0412', "CYRILLIC VE", "B"),
        ('\x0415', "CYRILLIC IE", "E"),
        ('\x041A', "CYRILLIC KA", "K"),
        ('\x041C', "CYRILLIC EM", "M"),
        ('\x041D', "CYRILLIC EN", "H"),
        ('\x041E', "CYRILLIC O", "O"),
        ('\x0420', "CYRILLIC ER", "P"),
        ('\x0421', "CYRILLIC ES", "C"),
        ('\x0422', "CYRILLIC TE", "T"),
        ('\x0425', "CYRILLIC HA", "X"),
        ('\x0430', "CYRILLIC SMALL A", "a"),
        ('\x043E', "CYRILLIC SMALL O", "o"),
        ('\x0440', "CYRILLIC SMALL ER", "p"),
        ('\x0441', "CYRILLIC SMALL ES", "c"),
        ('\x0445', "CYRILLIC SMALL HA", "x"),
        ('\x0455', "CYRILLIC SMALL DZE", "s")
      ]

-- offensive ranges
data GremlinRange = GremlinRange
  { rangeStart :: Int,
    rangeEnd :: Int,
    rangeName :: T.Text,
    baseChar :: Char -- the ASCII base character ('a' or 'A')
  }

gremlinRanges :: [GremlinRange]
gremlinRanges =
  [ GremlinRange
      { rangeStart = 0x1D622, -- 'a'
        rangeEnd = 0x1D63B, -- 'z'
        rangeName = "MATHEMATICAL SANS-SERIF ITALIC SMALL",
        baseChar = 'a'
      },
    GremlinRange
      { rangeStart = 0x1D608, -- 'A'
        rangeEnd = 0x1D621, -- 'Z'
        rangeName = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL",
        baseChar = 'A'
      }
  ]

data Hit
  = GlyphHit {lineNumber :: Int, colNumber :: Int, offender :: Offender}
  | EmojiHit {lineNumber :: Int, colNumber :: Int, seqText :: T.Text, reason :: T.Text}
  deriving (Show)

main :: IO ()
main = do
  args <- getArgs
  let isPedantic = "--pedantic" `elem` args

  input <- TIO.getContents
  let hits = scanText input
      totalHits = length hits
      totalChars = T.length input
      currentDensity = if totalChars == 0 then 0 else fromIntegral totalHits / fromIntegral totalChars :: Double

  if totalHits == 0
    then do
      putStrLn "FancyPolice: No Unicode gremlins found. Carry on, ASCII astronaut. \x1F9D1\x200D\x1F680\n"
      exitSuccess
    else do
      -- Always show the fun summary
      putStrLn "FancyPolice: Unicode shenanigans detected! 🕵️"
      printf "Found %d suspicious characters (%.2f%% density)\n\n" totalHits (currentDensity * 100)

      -- Show details only if pedantic OR threshold exceeded
      let shouldShowDetails = isPedantic || totalHits >= thresholds.absolute || currentDensity >= thresholds.density

      if shouldShowDetails
        then do
          when (totalHits >= thresholds.absolute) $
            printf "🚨  HIGH OFFENSE COUNT: %d violations detected!\n" totalHits
          when (currentDensity >= thresholds.density) $
            putStrLn "🚨  HIGH DENSITY: Too much Unicode glitter detected!"
          when shouldShowDetails $
            putStrLn "\nDetailed violations:"
          mapM_ printHit hits
        else do
          putStrLn "💡 Tip: Use --pedantic flag to see detailed violations"
          putStrLn "🎯 Or fix the major issues first (threshold exceeded = detailed report)"

      putStrLn ""
      if shouldShowDetails then exitFailure else exitSuccess

-- Pretty-print a percentage with 2 decimals
showPercent :: Double -> String
showPercent x = printf "%.2f%%" (x * 100)

scanText :: T.Text -> [Hit]
scanText t = concatMap scanLine (zip [1 ..] (T.lines t))
  where
    scanLine (ln, line) = scanUnits ln 1 line

-- Scan by extended units:
-- 1) Emoji ZWJ/VS16/skin-tone sequences (consume multiple codepoints)
-- 2) Single-char gremlins from the table
-- 3) Otherwise advance one char
scanUnits :: Int -> Int -> T.Text -> [Hit]
scanUnits _ _ s | T.null s = []
scanUnits ln col s =
  case takeEmojiCluster s of
    Just (cluster, rest) ->
      EmojiHit
        { lineNumber = ln,
          colNumber = col,
          seqText = cluster,
          reason = emojiWhy cluster
        }
        : scanUnits ln (col + 1) rest
    Nothing ->
      let c = T.head s
          rest = T.tail s
       in case M.lookup c gremlins of
            Just o ->
              GlyphHit
                { lineNumber = ln,
                  colNumber = col,
                  offender = o
                }
                : scanUnits ln (col + 1) rest
            Nothing ->
              case checkGremlinRanges c of
                Just (rangeName, suggestion) ->
                  GlyphHit
                    { lineNumber = ln,
                      colNumber = col,
                      offender = Offender {name = rangeName, suggestion = suggestion, culprit = c}
                    }
                    : scanUnits ln (col + 1) rest
                Nothing -> scanUnits ln (col + 1) rest

checkGremlinRanges :: Char -> Maybe (T.Text, T.Text)
checkGremlinRanges c =
  let codePoint = ord c
   in case filter (\r -> codePoint >= rangeStart r && codePoint <= rangeEnd r) gremlinRanges of
        (range : _) ->
          let suggestion = chr $ ord (baseChar range) + (codePoint - rangeStart range)
           in Just (rangeName range, T.singleton suggestion)
        [] -> Nothing

-- Why/fun message for emoji clusters
emojiWhy :: T.Text -> T.Text
emojiWhy cluster =
  let codePoints = T.pack . printf "U+%04X" . ord <$> T.unpack cluster
      uxs = T.intercalate ", " codePoints
   in T.pack $ printf "EMOJI sequence (%s) -> replace with ':emoji:' or ASCII, e.g., ':rocket:'." uxs

printHit :: Hit -> IO ()
printHit hit@GlyphHit {} =
  printf
    "- line %d, col %d: found \"%c\" (%s) [U+%04X] -> replace with \"%s\"\n"
    hit.lineNumber
    hit.colNumber
    hit.offender.culprit
    hit.offender.name
    (ord hit.offender.culprit)
    hit.offender.suggestion
printHit hit@EmojiHit {} =
  printf
    "- line %d, col %d: found \"%s\" (%s)\n"
    hit.lineNumber
    hit.colNumber
    hit.seqText
    hit.reason

debugChar :: Char -> IO ()
debugChar c = printf "Char '%c' has codepoint U+%04X (%d)\n" c (ord c) (ord c)
