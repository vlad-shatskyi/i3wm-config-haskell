module DataTypes.Other where

import Serializable

data WorkspaceNumber = W1 | W2 | W3 | W4 | W5 | W6 | W7 | W8 | W9 | W0

instance Serializable WorkspaceNumber where
  serialize W1 = "1"
  serialize W2 = "2"
  serialize W3 = "3"
  serialize W4 = "4"
  serialize W5 = "5"
  serialize W6 = "6"
  serialize W7 = "7"
  serialize W8 = "8"
  serialize W9 = "9"
  serialize W0 = "0"


data ModeName = ModeName String

instance Serializable ModeName where
  serialize (ModeName name) = "\"" ++ name ++ "\""


data GrowOrShrink = Grow | Shrink

instance Serializable GrowOrShrink where
  serialize Grow = "grow"
  serialize Shrink = "shrink"


data WidthOrHeight = Width | Height

instance Serializable WidthOrHeight where
  serialize Width = "width"
  serialize Height = "height"

data ShouldRelease = DontRelease | Release

instance Serializable ShouldRelease where
  serialize DontRelease = ""
  serialize Release = "--release"
