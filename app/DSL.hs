module DSL where

import DataTypes.Key
import Languages.I3
import DataTypes.Other
import Control.Monad.Free

class LH f g where
  lh :: (() -> f ()) -> Free g ()

instance LH TopLevelF TopLevelF where
  lh fx = liftF $ fx ()

instance LH BindingF BindingF where
  lh fx = liftF $ fx ()

instance LH LanguageF TopLevelF where
  lh fx = liftF $ LL (fx ())

instance LH BindingF TopLevelF where
  lh fx = liftF $ RR (fx ())

exec x = lh $ ExecStatement (toActionList (Exec x))
execAlways a = lh $ ExecAlways a
raw a = lh $ Raw a
font a b = lh $ Font a b

bindsym k a = lh $ BindingF (BindSym k (toActionList a))
bindcode s a = lh $ BindingF (BindCode DontRelease (shortcut s) (toActionList a))
s ==> a = bindcode s a

bar x = lh $ Bar x
hideEdgeBorders _ = lh HideEdgeBorders
forWindow criteria actions = lh $ ForWindow (ActionsWithCriteria criteria actions)

mode name config = lh $ ModeDefinition modeName bindings
  where modeName = ModeName name
        bindings = bindsym [EscapeSym] exit >> bindcode Q exit >> config

exit = ActivateMode (ModeName "default")

class ToActionList x where
  toActionList :: x -> ActionsWithCriteria

instance ToActionList Action where
  toActionList x = toActionList [x]

instance ToActionList [Action] where
  toActionList = ActionsWithCriteria []

instance ToActionList ActionsWithCriteria where
  toActionList = id

toBindingList :: Free BindingF a -> [Binding]
toBindingList = reverse . toList' []
  where toList' accumulator (Pure _) = accumulator
        toList' accumulator (Free (BindingF i3 next)) = toList' (i3:accumulator) next
