module Hoistable where

class (Functor g) => Hoistable f g where
  hoist :: f a -> g a

instance (Functor f) => Hoistable f f where
  hoist = id
