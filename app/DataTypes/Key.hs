{-# LANGUAGE QuasiQuotes #-}

module DataTypes.Key where

import Data.String.Interpolate

data KeyName = LowerVolumeSym
             | RaiseVolumeSym
             | MuteSym
             | BrightnessUpSym
             | BrightnessDownSym
             | EscapeSym
             | Mod4Sym
             | ShiftSym
             | SpaceSym
             | MinusSym
             | LeftBracketSym

data Key = Tilde
         | N1
         | N2
         | N3
         | N4
         | N5
         | N6
         | N7
         | N8
         | N9
         | N0
         | Minus
         | Equal
         | Q
         | W
         | E
         | R
         | T
         | Y
         | U
         | I
         | O
         | P
         | LeftBracket
         | RightBracket
         | Return
         | A
         | S
         | D
         | F
         | G
         | H
         | J
         | K
         | L
         | Semicolon
         | Quote
         | Z
         | X
         | C
         | V
         | B
         | N
         | M
         | Comma
         | Period
         | Slash


data Shortcut = NoModifier Key | Super Key | Shift Key | SuperShift Key | SuperCtrl Key
newtype OnRelease = OnRelease Key

instance Show KeyName where
  show = \case
    LowerVolumeSym -> "XF86AudioLowerVolume"
    RaiseVolumeSym -> "XF86AudioRaiseVolume"
    MuteSym -> "XF86AudioMute"
    BrightnessUpSym -> "XF86MonBrightnessUp"
    BrightnessDownSym -> "XF86MonBrightnessDown"
    EscapeSym -> "Escape"
    Mod4Sym -> "Mod4"
    ShiftSym -> "Shift"
    SpaceSym -> "space"
    MinusSym -> "minus"
    LeftBracketSym -> "bracketLeft"

keyCode :: Key -> Integer
keyCode = \case
  Tilde -> 49
  N1 -> 10
  N2 -> 11
  N3 -> 12
  N4 -> 13
  N5 -> 14
  N6 -> 15
  N7 -> 16
  N8 -> 17
  N9 -> 18
  N0 -> 19
  Minus -> 20
  Equal -> 21
  Q -> 24
  W -> 25
  E -> 26
  R -> 27
  T -> 28
  Y -> 29
  U -> 30
  I -> 31
  O -> 32
  P -> 33
  LeftBracket -> 34
  RightBracket -> 35
  Return -> 36
  A -> 38
  S -> 39
  D -> 40
  F -> 41
  G -> 42
  H -> 43
  J -> 44
  K -> 45
  L -> 46
  Semicolon -> 47
  Quote -> 48
  Z -> 52
  X -> 53
  C -> 54
  V -> 55
  B -> 56
  N -> 57
  M -> 58
  Comma -> 59
  Period -> 60
  Slash -> 61

instance Show Key where
  show = show . keyCode

instance Show Shortcut where
  show = \case
    NoModifier key -> show key
    Super key -> [i|Mod4+#{key}|]
    Shift key -> [i|Shift+#{key}|]
    SuperShift key -> [i|Mod4+Shift+#{key}|]
    SuperCtrl key -> [i|Mod4+Ctrl+#{key}|]
