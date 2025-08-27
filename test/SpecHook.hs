{-# LANGUAGE OverloadedStrings #-}

module SpecHook (hook) where

import Test.Hspec
import Test.Hspec.JUnit.Config (defaultJUnitConfig)
import qualified Test.Hspec.JUnit.Formatter as Formatter

hook :: Spec -> Spec
hook = Formatter.add $ defaultJUnitConfig "test-suite"
