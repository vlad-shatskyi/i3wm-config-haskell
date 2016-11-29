module DataTypes.Other where

data WorkspaceNumber = W1 | W2 | W3 | W4 | W5 | W6 | W7 | W8 | W9 | W0

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


data ModeName = ModeName String

instance Show ModeName where
  show = \case
    ModeName name -> "\"" ++ name ++ "\""


data GrowOrShrink = Grow | Shrink

instance Show GrowOrShrink where
  show = \case
    Grow -> "grow"
    Shrink -> "shrink"


data WidthOrHeight = Width | Height

instance Show WidthOrHeight where
  show = \case
    Width -> "width"
    Height -> "height"

data ShouldRelease = DontRelease | Release

instance Show ShouldRelease where
  show = \case
    DontRelease -> ""
    Release -> "--release"
