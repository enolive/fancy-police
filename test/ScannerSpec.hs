{-# LANGUAGE OverloadedStrings #-}

module ScannerSpec (spec) where

import qualified Data.Text as T
import Scanner (scanText)
import Test.Hspec
import Types

spec :: Spec
spec = describe "Scanner for Unicode input" $ do
  it "finds no hits on empty input" $ do
    let input = T.unlines 
            [ "This is a perfectly clean ASCII text file."
            , "It contains only regular letters, numbers 123, and symbols: !@#$%^&*()_+-=[]{}|;':\",./<>?"
            , "No fancy Unicode here - just plain old ASCII characters from 32 to 126."
            , "Even quotes are 'normal' and \"regular\" - no smart quotes!"
            , "Dashes are simple: - and not fancy em-dashes or en-dashes."
            , "Math uses basic operators: * / + - instead of fancy multiplication signs."
            , "Spaces are regular spaces, not no-break or thin spaces."
            , "This should produce zero hits from the fancy police scanner."
            ]
    let result = scanText input
    result `shouldBe` []

  it "finds no hits on input with plain ASCII" $ do
    let input = "abc"
    let result = scanText input
    result `shouldBe` []

  it "finds emojis" $ do
    -- use codepoints instead of emojis because some of them break the formatter
    let input = T.unlines ["Hi there \128512 from \127462\127476 with \128110!!", "0\65039\8419\x1F6A8 I guess"]
    let result = scanText input
    result
      `shouldBe` [ EmojiHit {lineNumber = 1, colNumber = 10, seqText = "\128512"},
                   EmojiHit {lineNumber = 1, colNumber = 17, seqText = "\127462\127476"},
                   EmojiHit {lineNumber = 1, colNumber = 24, seqText = "\128110"},
                   EmojiHit {lineNumber = 2, colNumber = 1, seqText = "0\65039\8419"},
                   EmojiHit {lineNumber = 2, colNumber = 2, seqText = "\x1F6A8"}
                 ]

  it "finds some gremlins" $ do
    let input = "this is a fact – 𝙄 𝙨𝙬𝙚𝙖𝙧 – that I’ve been there"
    let result = scanText input
    result
      `shouldBe` [ GlyphHit {lineNumber = 1, colNumber = 16, offender = Offender {name = "EN DASH", suggestion = "-", culprit = '\8211'}},
                   GlyphHit {lineNumber = 1, colNumber = 18, offender = Offender {name = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL", suggestion = "I", culprit = '\120388'}},
                   GlyphHit {lineNumber = 1, colNumber = 20, offender = Offender {name = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL", suggestion = "s", culprit = '\120424'}},
                   GlyphHit {lineNumber = 1, colNumber = 21, offender = Offender {name = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL", suggestion = "w", culprit = '\120428'}},
                   GlyphHit {lineNumber = 1, colNumber = 22, offender = Offender {name = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL", suggestion = "e", culprit = '\120410'}},
                   GlyphHit {lineNumber = 1, colNumber = 23, offender = Offender {name = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL", suggestion = "a", culprit = '\120406'}},
                   GlyphHit {lineNumber = 1, colNumber = 24, offender = Offender {name = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL", suggestion = "r", culprit = '\120423'}},
                   GlyphHit {lineNumber = 1, colNumber = 26, offender = Offender {name = "EN DASH", suggestion = "-", culprit = '\8211'}},
                   GlyphHit {lineNumber = 1, colNumber = 34, offender = Offender {name = "RIGHT SINGLE QUOTATION MARK", suggestion = "'", culprit = '\8217'}}
                 ]

  it "finds even more gremlins" $ do
    let input =
          T.unlines
            [ "𝙏𝙝𝙞𝙨 𝙞𝙨 𝘢 𝚝𝚎𝚜𝚝 – 𝗜 𝘀𝘸𝗲𝗮𝗿 – 𝒘𝒊𝒕𝒉 𝚖𝚊𝚗𝚢 𝒈𝒓𝒆𝒎𝒍𝒊𝒏𝒔! 😱",
              "𝐇𝐞𝐫𝐞'𝐬 𝘴𝘰𝘮𝘦 𝚖𝚘𝚗𝚘𝚜𝚙𝚊𝚌𝚎: 𝚟𝚊𝚛𝚒𝚊𝚋𝚕𝚎 = \"𝚏𝚞𝚗𝚌𝚝𝚒𝚘𝚗\"",
              "Αnd sοme Ϲyrillic: Μy νаmе іs Bοb",
              "Мath symbols: х ÷ у = z…",
              "Spaces→are→tricky  (that's an EM space)",
              "Quotes: \" Hello \" 'world' — plus some bullets • and ™ symbols"
            ]
    let result = scanText input
    result
      `shouldBe` [ GlyphHit {lineNumber = 1, colNumber = 1, offender = Offender {name = "MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL", suggestion = "T", culprit = '\120399'}},
                   GlyphHit {lineNumber = 1, colNumber = 2, offender = Offender {name = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL", suggestion = "h", culprit = '\120413'}},
                   GlyphHit {lineNumber = 1, colNumber = 3, offender = Offender {name = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL", suggestion = "i", culprit = '\120414'}},
                   GlyphHit {lineNumber = 1, colNumber = 4, offender = Offender {name = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL", suggestion = "s", culprit = '\120424'}},
                   GlyphHit {lineNumber = 1, colNumber = 6, offender = Offender {name = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL", suggestion = "i", culprit = '\120414'}},
                   GlyphHit {lineNumber = 1, colNumber = 7, offender = Offender {name = "MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL", suggestion = "s", culprit = '\120424'}},
                   GlyphHit {lineNumber = 1, colNumber = 9, offender = Offender {name = "MATHEMATICAL SANS-SERIF ITALIC SMALL", suggestion = "a", culprit = '\120354'}},
                   GlyphHit {lineNumber = 1, colNumber = 11, offender = Offender {name = "MATHEMATICAL MONOSPACE SMALL", suggestion = "t", culprit = '\120477'}},
                   GlyphHit {lineNumber = 1, colNumber = 12, offender = Offender {name = "MATHEMATICAL MONOSPACE SMALL", suggestion = "e", culprit = '\120462'}},
                   GlyphHit {lineNumber = 1, colNumber = 13, offender = Offender {name = "MATHEMATICAL MONOSPACE SMALL", suggestion = "s", culprit = '\120476'}},
                   GlyphHit {lineNumber = 1, colNumber = 14, offender = Offender {name = "MATHEMATICAL MONOSPACE SMALL", suggestion = "t", culprit = '\120477'}},
                   GlyphHit {lineNumber = 1, colNumber = 16, offender = Offender {name = "EN DASH", suggestion = "-", culprit = '\8211'}},
                   GlyphHit {lineNumber = 1, colNumber = 18, offender = Offender {name = "MATHEMATICAL SANS-SERIF BOLD CAPITAL", suggestion = "I", culprit = '\120284'}},
                   GlyphHit {lineNumber = 1, colNumber = 20, offender = Offender {name = "MATHEMATICAL SANS-SERIF BOLD SMALL", suggestion = "s", culprit = '\120320'}},
                   GlyphHit {lineNumber = 1, colNumber = 21, offender = Offender {name = "MATHEMATICAL SANS-SERIF ITALIC SMALL", suggestion = "w", culprit = '\120376'}},
                   GlyphHit {lineNumber = 1, colNumber = 22, offender = Offender {name = "MATHEMATICAL SANS-SERIF BOLD SMALL", suggestion = "e", culprit = '\120306'}},
                   GlyphHit {lineNumber = 1, colNumber = 23, offender = Offender {name = "MATHEMATICAL SANS-SERIF BOLD SMALL", suggestion = "a", culprit = '\120302'}},
                   GlyphHit {lineNumber = 1, colNumber = 24, offender = Offender {name = "MATHEMATICAL SANS-SERIF BOLD SMALL", suggestion = "r", culprit = '\120319'}},
                   GlyphHit {lineNumber = 1, colNumber = 26, offender = Offender {name = "EN DASH", suggestion = "-", culprit = '\8211'}},
                   GlyphHit {lineNumber = 1, colNumber = 33, offender = Offender {name = "MATHEMATICAL MONOSPACE SMALL", suggestion = "m", culprit = '\120470'}},
                   GlyphHit {lineNumber = 1, colNumber = 34, offender = Offender {name = "MATHEMATICAL MONOSPACE SMALL", suggestion = "a", culprit = '\120458'}},
                   GlyphHit {lineNumber = 1, colNumber = 35, offender = Offender {name = "MATHEMATICAL MONOSPACE SMALL", suggestion = "n", culprit = '\120471'}},
                   GlyphHit {lineNumber = 1, colNumber = 36, offender = Offender {name = "MATHEMATICAL MONOSPACE SMALL", suggestion = "y", culprit = '\120482'}},
                   EmojiHit {lineNumber = 1, colNumber = 48, seqText = "\128561"},
                   GlyphHit {lineNumber = 2, colNumber = 1, offender = Offender {name = "MATHEMATICAL BOLD CAPITAL", suggestion = "H", culprit = '\119815'}},
                   GlyphHit {lineNumber = 2, colNumber = 2, offender = Offender {name = "MATHEMATICAL BOLD SMALL", suggestion = "e", culprit = '\119838'}},
                   GlyphHit {lineNumber = 2, colNumber = 3, offender = Offender {name = "MATHEMATICAL BOLD SMALL", suggestion = "r", culprit = '\119851'}},
                   GlyphHit {lineNumber = 2, colNumber = 4, offender = Offender {name = "MATHEMATICAL BOLD SMALL", suggestion = "e", culprit = '\119838'}},
                   GlyphHit {lineNumber = 2, colNumber = 6, offender = Offender {name = "MATHEMATICAL BOLD SMALL", suggestion = "s", culprit = '\119852'}},
                   GlyphHit {lineNumber = 2, colNumber = 8, offender = Offender {name = "MATHEMATICAL SANS-SERIF ITALIC SMALL", suggestion = "s", culprit = '\120372'}},
                   GlyphHit {lineNumber = 2, colNumber = 9, offender = Offender {name = "MATHEMATICAL SANS-SERIF ITALIC SMALL", suggestion = "o", culprit = '\120368'}},
                   GlyphHit {lineNumber = 2, colNumber = 10, offender = Offender {name = "MATHEMATICAL SANS-SERIF ITALIC SMALL", suggestion = "m", culprit = '\120366'}},
                   GlyphHit {lineNumber = 2, colNumber = 11, offender = Offender {name = "MATHEMATICAL SANS-SERIF ITALIC SMALL", suggestion = "e", culprit = '\120358'}},
                   GlyphHit {lineNumber = 2, colNumber = 13, offender = Offender {name = "MATHEMATICAL MONOSPACE SMALL", suggestion = "m", culprit = '\120470'}},
                   GlyphHit {lineNumber = 2, colNumber = 14, offender = Offender {name = "MATHEMATICAL MONOSPACE SMALL", suggestion = "o", culprit = '\120472'}},
                   GlyphHit {lineNumber = 2, colNumber = 15, offender = Offender {name = "MATHEMATICAL MONOSPACE SMALL", suggestion = "n", culprit = '\120471'}},
                   GlyphHit {lineNumber = 2, colNumber = 16, offender = Offender {name = "MATHEMATICAL MONOSPACE SMALL", suggestion = "o", culprit = '\120472'}},
                   GlyphHit {lineNumber = 2, colNumber = 17, offender = Offender {name = "MATHEMATICAL MONOSPACE SMALL", suggestion = "s", culprit = '\120476'}},
                   GlyphHit {lineNumber = 2, colNumber = 18, offender = Offender {name = "MATHEMATICAL MONOSPACE SMALL", suggestion = "p", culprit = '\120473'}},
                   GlyphHit {lineNumber = 2, colNumber = 19, offender = Offender {name = "MATHEMATICAL MONOSPACE SMALL", suggestion = "a", culprit = '\120458'}},
                   GlyphHit {lineNumber = 2, colNumber = 20, offender = Offender {name = "MATHEMATICAL MONOSPACE SMALL", suggestion = "c", culprit = '\120460'}},
                   GlyphHit {lineNumber = 2, colNumber = 21, offender = Offender {name = "MATHEMATICAL MONOSPACE SMALL", suggestion = "e", culprit = '\120462'}},
                   GlyphHit {lineNumber = 2, colNumber = 24, offender = Offender {name = "MATHEMATICAL MONOSPACE SMALL", suggestion = "v", culprit = '\120479'}},
                   GlyphHit {lineNumber = 2, colNumber = 25, offender = Offender {name = "MATHEMATICAL MONOSPACE SMALL", suggestion = "a", culprit = '\120458'}},
                   GlyphHit {lineNumber = 2, colNumber = 26, offender = Offender {name = "MATHEMATICAL MONOSPACE SMALL", suggestion = "r", culprit = '\120475'}},
                   GlyphHit {lineNumber = 2, colNumber = 27, offender = Offender {name = "MATHEMATICAL MONOSPACE SMALL", suggestion = "i", culprit = '\120466'}},
                   GlyphHit {lineNumber = 2, colNumber = 28, offender = Offender {name = "MATHEMATICAL MONOSPACE SMALL", suggestion = "a", culprit = '\120458'}},
                   GlyphHit {lineNumber = 2, colNumber = 29, offender = Offender {name = "MATHEMATICAL MONOSPACE SMALL", suggestion = "b", culprit = '\120459'}},
                   GlyphHit {lineNumber = 2, colNumber = 30, offender = Offender {name = "MATHEMATICAL MONOSPACE SMALL", suggestion = "l", culprit = '\120469'}},
                   GlyphHit {lineNumber = 2, colNumber = 31, offender = Offender {name = "MATHEMATICAL MONOSPACE SMALL", suggestion = "e", culprit = '\120462'}},
                   GlyphHit {lineNumber = 2, colNumber = 36, offender = Offender {name = "MATHEMATICAL MONOSPACE SMALL", suggestion = "f", culprit = '\120463'}},
                   GlyphHit {lineNumber = 2, colNumber = 37, offender = Offender {name = "MATHEMATICAL MONOSPACE SMALL", suggestion = "u", culprit = '\120478'}},
                   GlyphHit {lineNumber = 2, colNumber = 38, offender = Offender {name = "MATHEMATICAL MONOSPACE SMALL", suggestion = "n", culprit = '\120471'}},
                   GlyphHit {lineNumber = 2, colNumber = 39, offender = Offender {name = "MATHEMATICAL MONOSPACE SMALL", suggestion = "c", culprit = '\120460'}},
                   GlyphHit {lineNumber = 2, colNumber = 40, offender = Offender {name = "MATHEMATICAL MONOSPACE SMALL", suggestion = "t", culprit = '\120477'}},
                   GlyphHit {lineNumber = 2, colNumber = 41, offender = Offender {name = "MATHEMATICAL MONOSPACE SMALL", suggestion = "i", culprit = '\120466'}},
                   GlyphHit {lineNumber = 2, colNumber = 42, offender = Offender {name = "MATHEMATICAL MONOSPACE SMALL", suggestion = "o", culprit = '\120472'}},
                   GlyphHit {lineNumber = 2, colNumber = 43, offender = Offender {name = "MATHEMATICAL MONOSPACE SMALL", suggestion = "n", culprit = '\120471'}},
                   GlyphHit {lineNumber = 3, colNumber = 1, offender = Offender {name = "GREEK ALPHA", suggestion = "A", culprit = '\913'}},
                   GlyphHit {lineNumber = 3, colNumber = 20, offender = Offender {name = "GREEK MU", suggestion = "M", culprit = '\924'}},
                   GlyphHit {lineNumber = 3, colNumber = 24, offender = Offender {name = "CYRILLIC SMALL A", suggestion = "a", culprit = '\1072'}},
                   GlyphHit {lineNumber = 4, colNumber = 1, offender = Offender {name = "CYRILLIC EM", suggestion = "M", culprit = '\1052'}},
                   GlyphHit {lineNumber = 4, colNumber = 15, offender = Offender {name = "CYRILLIC SMALL HA", suggestion = "x", culprit = '\1093'}},
                   GlyphHit {lineNumber = 4, colNumber = 24, offender = Offender {name = "HORIZONTAL ELLIPSIS", suggestion = "...", culprit = '\8230'}},
                   GlyphHit {lineNumber = 5, colNumber = 7, offender = Offender {name = "RIGHTWARDS ARROW", suggestion = "->", culprit = '\8594'}},
                   GlyphHit {lineNumber = 5, colNumber = 11, offender = Offender {name = "RIGHTWARDS ARROW", suggestion = "->", culprit = '\8594'}},
                   GlyphHit {lineNumber = 6, colNumber = 27, offender = Offender {name = "EM DASH", suggestion = "-", culprit = '\8212'}},
                   GlyphHit {lineNumber = 6, colNumber = 47, offender = Offender {name = "BULLET", suggestion = "*", culprit = '\8226'}},
                   GlyphHit {lineNumber = 6, colNumber = 53, offender = Offender {name = "TRADE MARK SIGN", suggestion = "(TM)", culprit = '\8482'}}
                 ]