module DataTypes.Key where

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
    EqualSym -> "equal"
    MinusSym -> "minus"
    LeftBracketSym -> "bracketLeft"

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


keyCode :: Key -> Integer
keyCode Tilde = 49
keyCode N1 = 10
keyCode N2 = 11
keyCode N3 = 12
keyCode N4 = 13
keyCode N5 = 14
keyCode N6 = 15
keyCode N7 = 16
keyCode N8 = 17
keyCode N9 = 18
keyCode N0 = 19
keyCode Minus = 20
keyCode Equal = 21
keyCode Q = 24
keyCode W = 25
keyCode E = 26
keyCode R = 27
keyCode T = 28
keyCode Y = 29
keyCode U = 30
keyCode I = 31
keyCode O = 32
keyCode P = 33
keyCode LeftBracket = 34
keyCode RightBracket = 35
keyCode Return = 36
keyCode A = 38
keyCode S = 39
keyCode D = 40
keyCode F = 41
keyCode G = 42
keyCode H = 43
keyCode J = 44
keyCode K = 45
keyCode L = 46
keyCode Semicolon = 47
keyCode Quote = 48
keyCode Z = 52
keyCode X = 53
keyCode C = 54
keyCode V = 55
keyCode B = 56
keyCode N = 57
keyCode M = 58
keyCode Comma = 59
keyCode Period = 60
keyCode Slash = 61

instance Show Key where
  show = show . keyCode

data Shortcut = NoModifier Key | Super Key | Shift Key | SuperShift Key | SuperCtrl Key

instance Show Shortcut where
  show = \case
    NoModifier key -> show key
    Super key -> "Mod4+" ++ show key
    Shift key -> "Shift+" ++ show key
    SuperShift key -> "Mod4+Shift+" ++ show key
    SuperCtrl key -> "Mod4+Ctrl+" ++ show key

class ToShortcut a where
  shortcut :: a -> Shortcut

instance ToShortcut Key where
  shortcut = NoModifier

instance ToShortcut Shortcut where
  shortcut = id
