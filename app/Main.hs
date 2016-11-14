module Main where

import Lib
import Control.Monad.Free
import Data.List (intercalate)

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

keyCode :: Key -> String
keyCode Q = "24"
keyCode W = "25"
keyCode E = "26"
keyCode R = "27"
keyCode T = "28"
keyCode Y = "29"
keyCode U = "30"
keyCode I = "31"
keyCode O = "32"
keyCode P = "33"
keyCode LeftBracket = "34"
keyCode RightBracket = "35"
keyCode Return = "36"
keyCode A = "38"
keyCode S = "39"
keyCode D = "40"
keyCode F = "41"
keyCode G = "42"
keyCode H = "43"
keyCode J = "44"
keyCode K = "45"
keyCode L = "46"
keyCode Semicolon = "47"
keyCode Quote = "48"
keyCode Z = "52"
keyCode X = "53"
keyCode C = "54"
keyCode V = "55"
keyCode B = "56"
keyCode N = "57"
keyCode M = "58"
keyCode Comma = "59"
keyCode Period = "60"
keyCode Slash = "61"
keyCode Mod4 = "Mod4"

data Exec = Exec String

data Op next
  = Action Exec next
  | ExecAlways String next
  | Font [String] Int next
  | BindCode [Key] Exec next
  deriving (Functor)

type Config = Free Op

exec :: String -> Config ()
exec x = liftF $ Action (Exec x) ()

exec_always :: String -> Config ()
exec_always x = liftF $ ExecAlways x ()

font :: [String] -> Int -> Config ()
font names size = liftF $ Font names size ()

bindcode :: [Key] -> Exec -> Config ()
bindcode keys command = liftF $ BindCode keys command ()

config :: Config ()
config = do
  exec_always "xinput set-prop 12 280 0" -- Disable Tapping Drag.
  exec_always "xinput set-prop 12 286 0.85" -- Increase Accel Speed.
  exec_always "xmodmap ~/.xmodmap"

  exec "google-chrome-unstable"
  exec "slack"
  exec "telegram-desktop"

  font ["pango", "monospace"] 8

  bindcode [Mod4, Return] (Exec "i3-sensible-terminal")

  return ()

interpret :: Config a -> String
interpret = interpret' ""
  where interpret' accumulator (Free (Action (Exec x) next)) = interpret' (accumulator ++ "exec " ++ x ++ "\n") next
        interpret' accumulator (Free (ExecAlways x next)) = interpret' (accumulator ++ "exec_always " ++ x ++ "\n") next
        interpret' accumulator (Free (Font names size next)) = interpret' (accumulator ++ "font " ++ (intercalate ":" names) ++ "\n") next
        interpret' accumulator (Free (BindCode codes (Exec command) next)) = interpret' (accumulator ++ "bindcode " ++ (intercalate "+" (map keyCode codes)) ++ " exec " ++ command ++ "\n") next
        interpret' accumulator (Pure _) = accumulator

main :: IO ()
main = putStrLn $ interpret config
