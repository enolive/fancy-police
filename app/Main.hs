{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (when)
import Data.Char (ord)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Exit (exitFailure, exitSuccess)
import Text.Printf (printf)
import Unicode.Emoji (takeEmojiCluster)

type Offender = (Char, T.Text, T.Text) -- (char, name, asciiSuggestion)

gremlins :: M.Map Char Offender
gremlins =
  M.fromList $
    fmap
      (\o@(c, _, _) -> (c, o))
      [ ('\x2013', "EN DASH U+2013", "-"),
        ('\x2014', "EM DASH U+2014", "-"),
        ('\x2212', "MINUS SIGN U+2212", "-"),
        ('\x201C', "LEFT DOUBLE QUOTATION MARK U+201C", "\""),
        ('\x201D', "RIGHT DOUBLE QUOTATION MARK U+201D", "\""),
        ('\x201E', "DOUBLE LOW-9 QUOTATION MARK U+201E", "\""), -- German low double quote „
        ('\x2018', "LEFT SINGLE QUOTATION MARK U+2018", "'"),
        ('\x2019', "RIGHT SINGLE QUOTATION MARK U+2019", "'"),
        ('\x2026', "HORIZONTAL ELLIPSIS U+2026", "..."),
        ('\x00A0', "NO-BREAK SPACE U+00A0", " "),
        ('\x2009', "THIN SPACE U+2009", " "),
        ('\x2002', "EN SPACE U+2002", " "),
        ('\x2003', "EM SPACE U+2003", " "),
        ('\x200B', "ZERO WIDTH SPACE U+200B", ""),
        ('\x200D', "ZERO WIDTH JOINER U+200D", ""),
        ('\x00D7', "MULTIPLICATION SIGN U+00D7", "*"),
        ('\x2215', "DIVISION SLASH U+2215", "/"),
        ('\x2044', "FRACTION SLASH U+2044", "/"),
        ('\x2192', "RIGHTWARDS ARROW U+2192", "->"),
        ('\x21D2', "RIGHTWARDS DOUBLE ARROW U+21D2", "=>"),
        ('\x2022', "BULLET U+2022", "*"),
        ('\x2122', "TRADE MARK SIGN U+2122", "(TM)"),
        ('\x00AE', "REGISTERED SIGN U+00AE", "(R)"),
        ('\x00B8', "CEDILLA U+00B8 (often paste artifact)", ","),
        ('\x1E9E', "LATIN CAPITAL LETTER SHARP S U+1E9E", "SS"),
        ('\x00B5', "MICRO SIGN U+00B5 (not Greek mu)", "u"),
        ('\x03BC', "GREEK SMALL LETTER MU U+03BC", "mu"),
        ('\x0391', "GREEK CAPITAL LETTER ALPHA U+0391 (looks like A)", "A"),
        ('\x0412', "CYRILLIC CAPITAL LETTER VE U+0412 (looks like B)", "B"),
        ('\x041E', "CYRILLIC CAPITAL LETTER O U+041E", "O"),
        ('\x0421', "CYRILLIC CAPITAL LETTER ES U+0421", "C"),
        ('\x0425', "CYRILLIC CAPITAL LETTER HA U+0425", "X")
      ]

data Hit
  = GlyphHit {lineNumber :: Int, colNo :: Int, ch :: Char, nameT :: T.Text, sug :: T.Text}
  | EmojiHit {lineNumber :: Int, colNo :: Int, seqText :: T.Text, reason :: T.Text}

main :: IO ()
main = do
  input <- TIO.getContents
  let hits = scanText input
      totalHits = length hits
      totalChars = T.length input
      density = if totalChars == 0 then 0 else fromIntegral totalHits / fromIntegral totalChars :: Double

      -- thresholds (tweak to taste)
      absThreshold = 40 -- absolute number of offenses
      densityThreshold = 0.03 -- 3% of all characters
  if totalHits == 0
    then do
      putStrLn "FancyPolice: No Unicode gremlins found. Carry on, ASCII astronaut. \x1F9D1\x200D\x1F680\n"
      exitSuccess
    else do
      putStrLn "FancyPolice: Halt! Typography bandits detected: ⚠️\n"

      -- Absolute-count alert
      when (totalHits >= absThreshold) $ do
        printf "SPECIAL ALERT: %d offenses detected — this is a full-on Unicode rodeo! \xF0\x9F\xA6\x80\n" totalHits
        putStrLn ""

      -- Density-based verdict
      printf "Offense density: %s (%d / %d chars)\n" (showPercent density) totalHits totalChars
      when (density >= densityThreshold) $
        putStrLn "Density alert: Too much Unicode glitter for comfort. Consider normalizing."

      putStrLn ""
      mapM_ printHit hits
      printf "\nTotal offenses: %d\n" totalHits
      exitFailure

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
          colNo = col,
          seqText = cluster,
          reason = emojiWhy cluster
        }
        : scanUnits ln (col + 1) rest
    Nothing ->
      let c = T.head s
          rest = T.tail s
       in case M.lookup c gremlins of
            Just (_, nameTxt, suggestion) ->
              GlyphHit
                { lineNumber = ln,
                  colNo = col,
                  ch = c,
                  nameT = nameTxt,
                  sug = suggestion
                }
                : scanUnits ln (col + 1) rest
            Nothing ->
              scanUnits ln (col + 1) rest

-- Why/fun message for emoji clusters
emojiWhy :: T.Text -> T.Text
emojiWhy cluster =
  let codePoints = T.pack . printf "U+%04X" . ord <$> T.unpack cluster
      uxs = T.intercalate ", " codePoints
   in T.pack $ printf "EMOJI sequence (%s). Suggestion: replace with ':emoji:' or ASCII, e.g., ':rocket:'." uxs

printHit :: Hit -> IO ()
printHit hit@GlyphHit {} =
  printf
    "- line %d, col %d: found \"%c\" (%s) [U+%04X] -> replace with \"%s\"\n"
    hit.lineNumber
    hit.colNo
    hit.ch
    hit.nameT
    (ord hit.ch)
    hit.sug
printHit hit@EmojiHit {} =
  printf
    "- line %d, col %d: found \"%s\" (%s)\n"
    hit.lineNumber
    hit.colNo
    hit.seqText
    hit.reason
