{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Scanner
  ( scanText,
    Hit (..),
    Offender (..),
  )
where

import Control.Applicative ((<|>))
import Data.Char (chr, ord)
import Data.List (find)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Unicode.Emoji ( takeEmojiCluster )
import Types

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
  { rangeStart :: Char,
    rangeEnd :: Char,
    rangeName :: T.Text,
    baseChar :: Char -- the ASCII base character ('a' or 'A')
  }

gremlinRanges :: [GremlinRange]
gremlinRanges =
  -- NOTE: there are several other ranges, but they are not used that often...
  [ GremlinRange
      { rangeStart = '\x1D622', -- 'a'
        rangeEnd = '\x1D63B', -- 'z'
        rangeName = "MATHEMATICAL SANS-SERIF ITALIC SMALL",
        baseChar = 'a'
      },
    GremlinRange
      { rangeStart = '\x1D608', -- 'A'
        rangeEnd = '\x1D621', -- 'Z'
        rangeName = "MATHEMATICAL SANS-SERIF ITALIC CAPITAL",
        baseChar = 'A'
      },
    GremlinRange
      { rangeStart = '\x1D5EE', -- 'a'
        rangeEnd = '\x1D607', -- 'z'
        rangeName = "MATHEMATICAL SANS-SERIF BOLD SMALL",
        baseChar = 'a'
      },
    GremlinRange
      { rangeStart = '\x1D5D4', -- 'A'
        rangeEnd = '\x1D5ED', -- 'Z'
        rangeName = "MATHEMATICAL SANS-SERIF BOLD CAPITAL",
        baseChar = 'A'
      },
    GremlinRange
      { rangeStart = '\x1D552', -- 'a'
        rangeEnd = '\x1D56B', -- 'z'
        rangeName = "MATHEMATICAL DOUBLE-STRUCK SMALL",
        baseChar = 'a'
      },
    -- NOTE: the double-struck capital characters are scattered around different codepoints and don't quite fit here...
    GremlinRange
      { rangeStart = '\x1D656', -- 'a'
        rangeEnd = '\x1D66F', -- 'z'
        rangeName = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL",
        baseChar = 'a'
      },
    GremlinRange
      { rangeStart = '\x1D63C', -- 'A'
        rangeEnd = '\x1D655', -- 'Z'
        rangeName = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL",
        baseChar = 'A'
      },
    GremlinRange
      { rangeStart = '\x1D41A', -- 'a'
        rangeEnd = '\x1D433', -- 'z'
        rangeName = "MATHEMATICAL BOLD SMALL",
        baseChar = 'a'
      },
    GremlinRange
      { rangeStart = '\x1D400', -- 'A'
        rangeEnd = '\x1D419', -- 'Z'
        rangeName = "MATHEMATICAL BOLD CAPITAL",
        baseChar = 'A'
      },
    GremlinRange
      { rangeStart = '\x1D68A', -- 'a'
        rangeEnd = '\x1D6A3', -- 'z'
        rangeName = "MATHEMATICAL MONOSPACE SMALL",
        baseChar = 'a'
      },
    GremlinRange
      { rangeStart = '\x1D670', -- 'A'
        rangeEnd = '\x1D689', -- 'Z'
        rangeName = "MATHEMATICAL MONOSPACE CAPITAL",
        baseChar = 'A'
      }
  ]

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
  case result of
    (Just hit, remaining) -> hit : scanUnits ln (col + nextCol hit) remaining
    (Nothing, remaining) -> scanUnits ln (col + 1) remaining
  where
    nextCol (GlyphHit {}) = 1
    nextCol (EmojiHit {seqText = text}) = T.length text
    result
      | Just (cluster, rest) <- takeEmojiCluster s =
          (Just $ EmojiHit {lineNumber = ln, colNumber = col, seqText = cluster}, rest)
      | Just o <- checkAnyGremlin (T.head s) =
          (Just $ GlyphHit {lineNumber = ln, colNumber = col, offender = o}, T.tail s)
      | otherwise = (Nothing, T.tail s)

checkAnyGremlin :: Char -> Maybe Offender
checkAnyGremlin c = checkSingleGremlin c <|> checkGremlinRanges c

checkSingleGremlin :: Char -> Maybe Offender
checkSingleGremlin c = M.lookup c gremlins

checkGremlinRanges :: Char -> Maybe Offender
checkGremlinRanges c = do
  let codePoint = ord c
  range <- find (\r -> c >= r.rangeStart && c <= r.rangeEnd) gremlinRanges
  let derivedSuggestion = T.singleton $ chr $ ord range.baseChar + (codePoint - ord range.rangeStart)
  return Offender {name = range.rangeName, suggestion = derivedSuggestion, culprit = c}