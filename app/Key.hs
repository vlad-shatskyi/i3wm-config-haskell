module Key where

import Serializable

data KeyName = LowerVolumeSym
             | RaiseVolumeSym
             | MuteSym
             | BrightnessUpSym
             | BrightnessDownSym
             | EscapeSym
             | Mod4Sym
             | ShiftSym
             | SpaceSym
             | EqualSym
             | MinusSym
             | LeftBracketSym

instance Serializable KeyName where
  serialize LowerVolumeSym = "XF86AudioLowerVolume"
  serialize RaiseVolumeSym = "XF86AudioRaiseVolume"
  serialize MuteSym = "XF86AudioMute"
  serialize BrightnessUpSym = "XF86MonBrightnessUp"
  serialize BrightnessDownSym = "XF86MonBrightnessDown"
  serialize EscapeSym = "Escape"
  serialize Mod4Sym = "Mod4"
  serialize ShiftSym = "Shift"
  serialize SpaceSym = "space"
  serialize EqualSym = "equal"
  serialize MinusSym = "minus"
  serialize LeftBracketSym = "bracketLeft"

data Key = Tilde
         | One
         | Two
         | Three
         | Four
         | Five
         | Six
         | Seven
         | Eight
         | Nine
         | Zero
         | Minus
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
         | Mod4
         | Shift

instance Serializable Key where
  serialize Tilde = "49"
  serialize Shift = "Shift"
  serialize One = "10"
  serialize Two = "11"
  serialize Three = "12"
  serialize Four = "13"
  serialize Five = "14"
  serialize Six = "15"
  serialize Seven = "16"
  serialize Eight = "17"
  serialize Nine = "18"
  serialize Zero = "19"
  serialize Minus = "20"
  serialize Q = "24"
  serialize W = "25"
  serialize E = "26"
  serialize R = "27"
  serialize T = "28"
  serialize Y = "29"
  serialize U = "30"
  serialize I = "31"
  serialize O = "32"
  serialize P = "33"
  serialize LeftBracket = "34"
  serialize RightBracket = "35"
  serialize Return = "36"
  serialize A = "38"
  serialize S = "39"
  serialize D = "40"
  serialize F = "41"
  serialize G = "42"
  serialize H = "43"
  serialize J = "44"
  serialize K = "45"
  serialize L = "46"
  serialize Semicolon = "47"
  serialize Quote = "48"
  serialize Z = "52"
  serialize X = "53"
  serialize C = "54"
  serialize V = "55"
  serialize B = "56"
  serialize N = "57"
  serialize M = "58"
  serialize Comma = "59"
  serialize Period = "60"
  serialize Slash = "61"
  serialize Mod4 = "Mod4"
