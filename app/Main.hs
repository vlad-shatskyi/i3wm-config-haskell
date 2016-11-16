module Main where

import Lib
import Control.Monad.Free
import Data.List (intercalate)

data CommandCriteria = Instance String
                     | Class String
                     | Title String

instance Show CommandCriteria where
  show (Instance name) = "instance=\"" ++ name ++ "\""
  show (Class name) = "class=\"" ++ name ++ "\""
  show (Title name) = "title=\"" ++ name ++ "\""

instance Show [CommandCriteria] where
  show criteria = "[" ++ intercalate " " (map show criteria) ++ "]"

data KeyName = LowerVolume
             | RaiseVolume
             | Mute
             | BrightnessUp
             | BrightnessDown


instance Show KeyName where
  show LowerVolume = "XF86AudioLowerVolume"
  show RaiseVolume = "XF86AudioRaiseVolume"
  show Mute = "XF86AudioMute"
  show BrightnessUp = "XF86MonBrightnessUp"
  show BrightnessDown = "XF86MonBrightnessDown"

data Key = Q
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

instance Show Key where
  show Q = "24"
  show W = "25"
  show E = "26"
  show R = "27"
  show T = "28"
  show Y = "29"
  show U = "30"
  show I = "31"
  show O = "32"
  show P = "33"
  show LeftBracket = "34"
  show RightBracket = "35"
  show Return = "36"
  show A = "38"
  show S = "39"
  show D = "40"
  show F = "41"
  show G = "42"
  show H = "43"
  show J = "44"
  show K = "45"
  show L = "46"
  show Semicolon = "47"
  show Quote = "48"
  show Z = "52"
  show X = "53"
  show C = "54"
  show V = "55"
  show B = "56"
  show N = "57"
  show M = "58"
  show Comma = "59"
  show Period = "60"
  show Slash = "61"
  show Mod4 = "Mod4"

data Exec = Exec String

instance Show Exec where
  show (Exec x) = "exec " ++ x

data I3 = Action Exec
        | ExecAlways String
        | Font [String] Int
        | BindSym [KeyName] Exec
        | BindCode [Key] Exec
        | Bar String
        | HideEdgeBorders
        | ForWindow CommandCriteria Exec

instance Show I3 where
  show (Action exec) = show exec
  show (ExecAlways x) = "exec_always " ++ x
  show (Font names size) = "font " ++ (intercalate ":" names)
  show (BindSym keys exec) = "bindsym " ++ (intercalate "+" (map show keys)) ++ " " ++ show exec
  show (BindCode codes exec) = "bindcode " ++ (intercalate "+" (map show codes)) ++ " " ++ show exec
  show (Bar command) = "bar {\n    status_command " ++ command ++ "\n    position top\n}"
  show HideEdgeBorders = "hide_edge_borders both"

data Op next = Op I3 next deriving (Functor)

type Config = Free Op

exec :: String -> Config ()
exec x = liftF $ Op (Action (Exec x)) ()

exec_always :: String -> Config ()
exec_always x = liftF $ Op (ExecAlways x) ()

font :: [String] -> Int -> Config ()
font names size = liftF $ Op (Font names size) ()

bindsym :: [KeyName] -> Exec -> Config ()
bindsym keys command = liftF $ Op (BindSym keys command) ()

bindcode :: [Key] -> Exec -> Config ()
bindcode keys command = liftF $ Op (BindCode keys command) ()

bar :: String -> Config ()
bar command = liftF $ Op (Bar command) ()

hide_edge_borders :: Config ()
hide_edge_borders = liftF $ Op HideEdgeBorders ()

chrome = [Instance "google-chrome-unstable"]
rubymine = [Class "jetbrains-rubymine"]
slack = [Instance "slack"]
telegram = [Title "Telegram"]
terminal = [Instance "urxvt"]

config :: Config ()
config = do
  exec_always "xinput set-prop 12 280 0" -- Disable Tapping Drag.
  exec_always "xinput set-prop 12 286 0.85" -- Increase Accel Speed.
  exec_always "xmodmap ~/.xmodmap"

  exec "google-chrome-unstable"
  exec "slack"
  exec "telegram-desktop"

  font ["pango", "monospace"] 8

  bar "i3blocks"
  hide_edge_borders

  bindsym [RaiseVolume] (Exec "amixer -q sset Master 5%+ unmute")
  bindsym [LowerVolume] (Exec "amixer -q sset Master 5%- unmute")
  bindsym [Mute] (Exec "amixer -q sset Master,0 toggle")

  bindsym [BrightnessUp] (Exec "xbacklight -inc 10")
  bindsym [BrightnessDown] (Exec "xbacklight -dec 10")

  bindcode [Mod4, Return] (Exec "i3-sensible-terminal")

  return ()

interpret :: Config a -> String
interpret = interpret' ""
  where interpret' accumulator (Pure _) = accumulator
        interpret' accumulator (Free (Op i3 next)) = interpret' (accumulator ++ show i3 ++ "\n") next

main :: IO ()
main = putStrLn $ interpret config
