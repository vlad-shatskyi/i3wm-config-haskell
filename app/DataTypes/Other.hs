{-# LANGUAGE QuasiQuotes #-}

module DataTypes.Other where

import Data.String.Interpolate

data WorkspaceNumber = W1 | W2 | W3 | W4 | W5 | W6 | W7 | W8 | W9 | W0
data ModeName = ModeName String
data GrowOrShrink = Grow | Shrink
data WidthOrHeight = Width | Height
data ShouldRelease = DontRelease | Release

instance Show WorkspaceNumber where
  show = \case
    W1 -> "1"
    W2 -> "2"
    W3 -> "3"
    W4 -> "4"
    W5 -> "5"
    W6 -> "6"
    W7 -> "7"
    W8 -> "8"
    W9 -> "9"
    W0 -> "0"

instance Show ModeName where
  show = \case
    ModeName name -> [i|"#{name}"|]

instance Show GrowOrShrink where
  show = \case
    Grow -> "grow"
    Shrink -> "shrink"

instance Show WidthOrHeight where
  show = \case
    Width -> "width"
    Height -> "height"

instance Show ShouldRelease where
  show = \case
    DontRelease -> ""
    Release -> "--release"
