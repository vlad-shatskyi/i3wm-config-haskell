module Languages.I4 where

import qualified Languages.I3 as I3
import DataTypes.Other
import Hoistable

import Control.Monad.Free

data I4 = I4 I3.I3
        | ModeDefinition ModeName [I4]

data LanguageF next = LanguageF I4 next deriving (Functor)

instance Hoistable I3.LanguageF LanguageF where
  hoist (I3.LanguageF statement next) = LanguageF (I4 statement) next

instance Hoistable I3.BindingF LanguageF where
  hoist = (hoist :: I3.LanguageF a -> LanguageF a) . hoist

interpretLanguageF :: LanguageF (Free I3.LanguageF a) -> Free I3.LanguageF a
interpretLanguageF (LanguageF (I4 i3) next) = liftF (I3.LanguageF i3 ()) >> next
interpretLanguageF (LanguageF _ next) = next -- FIXME.
