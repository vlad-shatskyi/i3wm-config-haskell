module DSL where

import DataTypes.Key
import Languages.I3
import qualified Languages.I4 as I4
import DataTypes.Other
import Hoistable
import Control.Monad.Free

lh fx = hoistFree hoist $ liftF $ fx ()

actionList' :: [ActionCriteria] -> [Action] -> ActionList
actionList' cs xs = ActionList [ActionsWithCriteria cs xs]

action' :: [ActionCriteria] -> Action -> ActionList
action' cs x = actionList' cs [x]

exec x = lh $ LanguageF $ ExecStatement (toActionList (Exec x))
execAlways a = lh $ LanguageF $ ExecAlways a
raw a = lh $ LanguageF $ Raw a
font a b = lh $ LanguageF $ Font a b

bindsym k a = lh $ BindingF (BindSym k (toActionList a))
bindcode s a = lh $ BindingF (BindCode DontRelease (shortcut s) (toActionList a))
s ==> a = bindcode s a

bar x = lh $ LanguageF $ Bar x
hideEdgeBorders _ = lh $ LanguageF HideEdgeBorders
forWindow criteria actions = lh $ LanguageF (ForWindow (ActionsWithCriteria criteria actions))

mode name config = lh $ LanguageF (ModeDefinition modeName bindings)
  where modeName = ModeName name
        bindings = toBindingList $ bindsym [EscapeSym] exit >> bindcode Q exit >> config

exit = ActivateMode (ModeName "default")

class ToActionList x where
  toActionList :: x -> ActionList

instance ToActionList Action where
  toActionList x = toActionList [x]

instance ToActionList [Action] where
  toActionList xs = ActionList [ActionsWithCriteria [] xs]

instance ToActionList ActionList where
  toActionList = id

toBindingList :: Free BindingF a -> [Binding]
toBindingList = reverse . toList' []
  where toList' accumulator (Pure _) = accumulator
        toList' accumulator (Free (BindingF i3 next)) = toList' (i3:accumulator) next
