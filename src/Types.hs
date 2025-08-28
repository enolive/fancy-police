{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Types where

import qualified Data.Text as T
import Data.Yaml (FromJSON)
import GHC.Generics (Generic)

data Offender = Offender
  { name :: T.Text,
    suggestion :: T.Text,
    culprit :: Char
  }
  deriving (Show, Eq)

data Hit
  = GlyphHit {lineNumber :: Int, colNumber :: Int, offender :: Offender}
  | EmojiHit {lineNumber :: Int, colNumber :: Int, seqText :: T.Text}
  deriving (Show, Eq)

data Thresholds = Thresholds
  { absolute :: Int,
    density :: Double
  }
  deriving (Generic, Show, FromJSON)

data ReportMode = Brief | Pedantic
  deriving (Eq, Show, Bounded, Enum)