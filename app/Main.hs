module Main where

import Lib
import Control.Monad.Free

data Op next
  = Exec String next
  | ExecAlways String next
  deriving (Functor)

type Config = Free Op

exec :: String -> Config ()
exec x = liftF $ Exec x ()

exec_always :: String -> Config ()
exec_always x = liftF $ ExecAlways x ()

config :: Config ()
config = do
  exec_always "xinput set-prop 12 280 0" -- Disable Tapping Drag.
  exec_always "xinput set-prop 12 286 0.85" -- Increase Accel Speed.
  exec_always "xmodmap ~/.xmodmap"

  exec "google-chrome-unstable"
  exec "slack"
  exec "telegram-desktop"

  return ()

interpret :: Config a -> String
interpret = interpret' ""
  where interpret' accumulator (Free (Exec x next)) = interpret' (accumulator ++ "exec " ++ x ++ "\n") next
        interpret' accumulator (Free (ExecAlways x next)) = interpret' (accumulator ++ "exec_always " ++ x ++ "\n") next
        interpret' accumulator (Pure _) = accumulator

main :: IO ()
main = putStrLn $ interpret config
