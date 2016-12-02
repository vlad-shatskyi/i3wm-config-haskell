module Hoistable where

class (Functor g) => Hoistable f g where
  hoist :: f b -> g b

instance (Functor f) => Hoistable f f where
  hoist = id
