module Main where

import Lib
import Control.Monad.Free

data Op next
  = Exec String next
  | End
  deriving (Functor)


type Config = Free Op

exec :: String -> Config ()
exec x = liftF $ Exec x ()

end :: Config a
end = liftF End


config :: Config a
config = do
  exec "google-chrome-unstable"
  end

interpret :: Config a -> String
interpret = interpret' ""
  where interpret' accumulator (Free (Exec x next)) = interpret' (accumulator ++ "exec " ++ x ++ "\n") next
        interpret' accumulator (Free End) = accumulator

main :: IO ()
main = putStrLn $ interpret config
