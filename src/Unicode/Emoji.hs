module Unicode.Emoji (takeEmojiCluster) where

import Control.Monad (guard)
import Data.Char (ord)
import qualified Data.Text as T

-- Very lightweight emoji cluster detector.
-- Not a full grapheme breaker, but catches most modern emoji cases:
-- - Base emoji scalar (from several blocks)
-- - Optional VS16 U+FE0F
-- - Optional skin tone U+1F3FB..U+1F3FF
-- - ZWJ-joined sequences (... U+200D emoji)+
-- - Regional indicator pairs (flags)
-- - keycaps (numbers followed by emojis)
takeEmojiCluster :: T.Text -> Maybe (T.Text, T.Text)
takeEmojiCluster s
  | T.null s = Nothing
  | Just t <- takeKeycapSequence s = Just t
  | Just t <- takeRegionalFlag s = Just t
  | Just t <- takeRegularEmoji s = Just t
  | otherwise = Nothing

-- Takes a regular emoji, i.e. one that starts with a base emoji scalar
takeRegularEmoji :: T.Text -> Maybe (T.Text, T.Text)
takeRegularEmoji s = do
  (c, rest) <- T.uncons s
  guard (isEmojiScalar c)
  let (seq1, r1) = takeOptVS16 rest
      (seq2, r2) = takeOptSkin seq1 r1
      (joined, rJ) = takeZWJChain (T.cons c seq2) r2
  return (joined, rJ)

-- takes an emoji that starts with a number such as 1️⃣ or *️⃣
takeKeycapSequence :: T.Text -> Maybe (T.Text, T.Text)
takeKeycapSequence s = do
  (base, r1) <- T.uncons s
  guard (isKeycapBase base)
  (vs16, r2) <- T.uncons r1
  guard (ord vs16 == variationSelector16)
  (keycap, r3) <- T.uncons r2
  guard (ord keycap == combiningEnclosingKeycap)
  pure (T.pack [base, vs16, keycap], r3)
  where
    isKeycapBase c = c `elem` (['0' .. '9'] ++ ['*', '#'])

-- Regional indicator flags: two consecutive code points in U+1F1E6..U+1F1FF
takeRegionalFlag :: T.Text -> Maybe (T.Text, T.Text)
takeRegionalFlag s = do
  (a, r1) <- T.uncons s
  (b, r2) <- T.uncons r1
  guard $ isRegional a && isRegional b
  return (T.pack [a, b], r2)

isRegional :: Char -> Bool
isRegional c = inRange 0x1F1E6 0x1F1FF (ord c)

-- Emoji presentation selector
takeOptVS16 :: T.Text -> (T.Text, T.Text)
takeOptVS16 s =
  case T.uncons s of
    Just (c, r) | ord c == variationSelector16 -> (T.singleton c, r)
    _ -> (T.empty, s)

-- Fitzpatrick skin tones
takeOptSkin :: T.Text -> T.Text -> (T.Text, T.Text)
takeOptSkin acc s =
  case T.uncons s of
    Just (c, r)
      | let n = ord c,
        inRange 0x1F3FB 0x1F3FF n ->
          (acc `T.snoc` c, r)
    _ -> (acc, s)

-- Chain of (ZWJ + emoji-segment), for instance for family or astronaut
takeZWJChain :: T.Text -> T.Text -> (T.Text, T.Text)
takeZWJChain acc s =
  case parseZWJSegment s of
    Just (segment, rest) -> takeZWJChain (acc <> segment) rest
    Nothing -> (acc, s)
  where
    parseZWJSegment input = do
      (zwj, r1) <- T.uncons input
      guard (ord zwj == zeroWidthJoiner)
      (c2, r2) <- T.uncons r1
      guard (isEmojiScalar c2)
      let (vs, rVS) = takeOptVS16 r2
          (sk, rSK) = takeOptSkin vs rVS
      return (T.cons zwj (T.cons c2 sk), rSK)

-- Heuristic: treat these blocks/ranges as emoji-capable scalars
isEmojiScalar :: Char -> Bool
isEmojiScalar c =
  let n = ord c
   in any
        ($ n)
        [ inRange 0x231A 0x231B, -- watch/hourglass
          inRange 0x23E9 0x23FA, -- media controls, record
          inRange 0x25A0 0x25FF, -- Add this: Geometric Shapes
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

variationSelector16 :: Int
variationSelector16 = 0xFE0F

zeroWidthJoiner :: Int
zeroWidthJoiner = 0x200D

combiningEnclosingKeycap :: Int
combiningEnclosingKeycap = 0x20E3