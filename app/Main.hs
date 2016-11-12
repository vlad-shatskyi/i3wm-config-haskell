module Main where

import Lib
import Control.Monad.Free

data Op next
  = Exec String next
  deriving (Functor)


type Config = Free Op

exec :: String -> Config ()
exec x = liftF $ Exec x ()

config :: Config ()
config = do
  exec "google-chrome-unstable"
  return ()

interpret :: Config a -> String
interpret = interpret' ""
  where interpret' accumulator (Free (Exec x next)) = interpret' (accumulator ++ "exec " ++ x ++ "\n") next
        interpret' accumulator (Pure _) = accumulator

main :: IO ()
main = putStrLn $ interpret config
