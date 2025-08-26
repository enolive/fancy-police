module Unicode.Emoji (takeEmojiCluster) where
    
import qualified Data.Text as T
import Data.Char (ord)

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