{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (when)
import Data.Char (ord)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Exit (exitFailure, exitSuccess)

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
  = GlyphHit {lineNo :: Int, colNo :: Int, ch :: Char, nameT :: T.Text, sug :: T.Text}
  | EmojiHit {lineNo :: Int, colNo :: Int, seqText :: T.Text, reason :: T.Text}

main :: IO ()
main = do
  input <- TIO.getContents
  let hits       = scanText input
      totalHits  = length hits
      totalChars = T.length input
      density    = if totalChars == 0 then 0 else fromIntegral totalHits / fromIntegral totalChars :: Double

      -- thresholds (tweak to taste)
      absThreshold      = 5        -- absolute number of offenses
      densityThreshold  = 0.002    -- 0.2% of all characters

  if totalHits == 0
    then do
      putStrLn $
        "FancyPolice: No Unicode gremlins found. Carry on, ASCII astronaut. "
          ++ T.unpack "\x1F9D1\x200D\x1F680"
      exitSuccess
    else do
      putStrLn "FancyPolice: Halt! Typography bandits detected:\n"

      -- Absolute-count alert
      when (totalHits >= absThreshold) $ do
        putStrLn $ "SPECIAL ALERT: " ++ show totalHits ++ " offenses detected — this is a full-on Unicode rodeo! \xF0\x9F\xA6\x80"
        putStrLn ""

      -- Density-based verdict
      putStrLn $ "Offense density: " ++ showPercent density ++ " (" ++ show totalHits ++ " / " ++ show totalChars ++ " chars)"
      when (density >= densityThreshold) $
        putStrLn "Density alert: Too much Unicode glitter for comfort. Consider normalizing."

      putStrLn ""
      mapM_ printHit hits
      putStrLn $ "\nTotal offenses: " ++ show totalHits
      exitFailure

-- Pretty-print a percentage with 2 decimals
showPercent :: Double -> String
showPercent x = pad 2 (x * 100) <> "%"
  where
    pad decPlaces v =
      let s = showFFixed decPlaces v
          -- ensure at least minIntPlaces to the left if you ever want padding; 0 here means "as-is"
      in s

-- Minimal fixed-decimal formatter (2 decimals) without imports:
showFFixed :: Int -> Double -> String
showFFixed decPlaces v =
  let factor = (10 :: Int) ^ decPlaces
      n      = if v < 0 then -v else v
      whole  = floor n :: Integer
      frac   = floor ((n - fromIntegral whole) * fromIntegral factor + 0.5) :: Integer
      sign   = if v < 0 then "-" else ""
      fracStr =
        let s = show frac
        in replicate (decPlaces - length s) '0' ++ s
  in sign ++ show whole ++ "." ++ fracStr

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
      -- Count one column for the whole emoji cluster
      EmojiHit ln col cluster (emojiWhy cluster) : scanUnits ln (col + 1) rest
    Nothing ->
      let c = T.head s
          rest = T.tail s
       in case M.lookup c gremlins of
            Just (_, nameTxt, suggestion) ->
              GlyphHit ln col c nameTxt suggestion : scanUnits ln (col + 1) rest
            Nothing ->
              scanUnits ln (col + 1) rest

-- Very lightweight emoji cluster detector.
-- Not a full grapheme breaker, but catches most modern emoji cases:
-- - Base emoji scalar (from several blocks)
-- - Optional VS16 U+FE0F
-- - Optional skin tone U+1F3FB..U+1F3FF
-- - ZWJ-joined sequences (... U+200D emoji)+
-- - Regional indicator pairs (flags)
takeEmojiCluster :: T.Text -> Maybe (T.Text, T.Text)
takeEmojiCluster s
  | T.null s = Nothing
  | Just t <- takeRegionalFlag s = Just t
  | otherwise =
      case T.uncons s of
        Just (c, rest)
          | isEmojiScalar c ->
              let (seq1, r1) = takeOptVS16 rest
                  (seq2, r2) = takeOptSkin seq1 r1
                  (joined, rJ) = takeZWJChain (T.cons c seq2) r2
               in Just (joined, rJ)
        _ -> Nothing

-- Regional indicator flags: two consecutive code points in U+1F1E6..U+1F1FF
takeRegionalFlag :: T.Text -> Maybe (T.Text, T.Text)
takeRegionalFlag s = do
  (a, r1) <- T.uncons s
  (b, r2) <- T.uncons r1
  if isRegional a && isRegional b
    then Just (T.pack [a, b], r2)
    else Nothing

isRegional :: Char -> Bool
isRegional c = let n = ord c in n >= 0x1F1E6 && n <= 0x1F1FF

-- Emoji presentation selector
takeOptVS16 :: T.Text -> (T.Text, T.Text)
takeOptVS16 s =
  case T.uncons s of
    Just (c, r) | ord c == 0xFE0F -> (T.singleton c, r)
    _ -> (T.empty, s)

-- Fitzpatrick skin tones
takeOptSkin :: T.Text -> T.Text -> (T.Text, T.Text)
takeOptSkin acc s =
  case T.uncons s of
    Just (c, r)
      | let n = ord c,
        n >= 0x1F3FB && n <= 0x1F3FF ->
          (acc `T.snoc` c, r)
    _ -> (acc, s)

-- Chain of (ZWJ + emoji-segment)
takeZWJChain :: T.Text -> T.Text -> (T.Text, T.Text)
takeZWJChain acc s =
  case T.uncons s of
    Just (zwj, r1) | ord zwj == 0x200D ->
      case T.uncons r1 of
        Just (c2, r2)
          | isEmojiScalar c2 ->
              let (vs, rVS) = takeOptVS16 r2
                  (sk, rSK) = takeOptSkin vs rVS
               in takeZWJChain (acc `T.snoc` zwj `T.snoc` c2 <> sk) rSK
        _ -> (acc, s) -- dangling ZWJ; stop
    _ -> (acc, s)

-- Heuristic: treat these blocks/ranges as emoji-capable scalars
isEmojiScalar :: Char -> Bool
isEmojiScalar c =
  let n = ord c
   in any
        ($ n)
        [ inRange 0x231A 0x231B, -- watch/hourglass
          inRange 0x23E9 0x23FA, -- media controls, record
          inRange 0x2600 0x27BF, -- misc symbols + dingbats
          inRange 0x2934 0x2935, -- arrows
          inRange 0x2B05 0x2BFF, -- arrows etc.
          inRange 0x3030 0x303D, -- wavy dash, part alternation mark
          inRange 0x3297 0x3299, -- circled ideographs
          inRange 0x1F004 0x1F9FF, -- Mahjong tile through people, food, places, objects
          inRange 0x1FA70 0x1FAFF, -- symbols & pictographs extended-A
          inRange 0x1F000 0x1F0FF -- playing cards etc.
        ]

inRange :: Int -> Int -> Int -> Bool
inRange a b x = x >= a && x <= b

-- Why/fun message for emoji clusters
emojiWhy :: T.Text -> T.Text
emojiWhy cluster =
  let uxs =
        T.intercalate (T.pack ", ") $
          fmap ((T.pack . (\n -> "U+" ++ hex4 n)) . ord) (T.unpack cluster)
   in "EMOJI sequence (" <> uxs <> "). Suggestion: replace with ':emoji:' or ASCII, e.g., ':rocket:'."

printHit :: Hit -> IO ()
printHit (GlyphHit ln col c nameTxt suggestion) =
  putStrLn $
    "- line "
      ++ show ln
      ++ ", col "
      ++ show col
      ++ ": found \""
      ++ [c]
      ++ "\" ("
      ++ T.unpack nameTxt
      ++ ") [U+"
      ++ hex4 (ord c)
      ++ "] -> replace with \""
      ++ T.unpack suggestion
      ++ "\""
printHit (EmojiHit ln col cluster why) =
  putStrLn $
    "- line "
      ++ show ln
      ++ ", col "
      ++ show col
      ++ ": found "
      ++ show (T.unpack cluster)
      ++ " "
      ++ "("
      ++ T.unpack why
      ++ ")"

-- Small hex helper
hex4 :: Int -> String
hex4 n =
  let h = showHex n
   in replicate (4 - length h) '0' ++ h

showHex :: Int -> String
showHex n
  | n < 16 = [hexDigit n]
  | otherwise = showHex (n `div` 16) ++ [hexDigit (n `mod` 16)]
  where
    hexDigit d = "0123456789ABCDEF" !! d
