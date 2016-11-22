module Serializable where

class Serializable a where
  serialize :: a -> String
