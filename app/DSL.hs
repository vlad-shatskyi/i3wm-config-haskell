module DSL where

import DataTypes.Key
import Languages.I3
import DataTypes.Other
import Control.Monad.Free

data BindingF next = BindingF Binding next deriving (Functor)
data StatementF next = StatementF Statement next deriving (Functor)

liftF' :: Statement -> Free StatementF ()
liftF' x = liftF $ StatementF x ()

liftF'' = (liftF' .)

actionList' :: [ActionCriteria] -> [Action] -> ActionList
actionList' cs xs = ActionList [ActionsWithCriteria cs xs]

action' :: [ActionCriteria] -> Action -> ActionList
action' cs x = actionList' cs [x]

exec x = liftF' (ExecStatement (toActionList (Exec x)))
execAlways = liftF' . ExecAlways
raw = liftF' . Raw
font = liftF'' . Font

bindsym :: (ActionListConvertible a) => [KeyName] -> a -> Free BindingF ()
bindsym k a= liftF $ BindingF (Binding (Left (BindSym k)) (toActionList a)) ()

bindcode :: (ToShortcut s, ActionListConvertible a) => s -> a -> Free BindingF ()
bindcode s a = liftF $ BindingF (Binding (Right (BindCode DontRelease (shortcut s))) (toActionList a)) ()

a ==> b = bindcode a b

bar = liftF' . Bar
hideEdgeBorders = liftF' HideEdgeBorders
forWindow criteria actions = liftF' (ForWindow (ActionsWithCriteria criteria actions))

mode name config = liftF' (ModeDefinition modeName modeStatements)
  where modeName = ModeName name
        modeStatements = toBindingList (bindsym [EscapeSym] exit >> config)

exit = ActivateMode (ModeName "default")

class ActionListConvertible x where
  toActionList :: x -> ActionList

instance ActionListConvertible Action where
  toActionList x = toActionList [x]

instance ActionListConvertible [Action] where
  toActionList xs = ActionList [ActionsWithCriteria [] xs]

instance ActionListConvertible ActionList where
  toActionList = id

toList :: Free StatementF a -> [Statement]
toList = reverse . toList' []
  where toList' accumulator (Pure _) = accumulator
        toList' accumulator (Free (StatementF i3 next)) = toList' (i3:accumulator) next

toBindingList :: Free BindingF a -> [Binding]
toBindingList = reverse . toList' []
  where toList' accumulator (Pure _) = accumulator
        toList' accumulator (Free (BindingF i3 next)) = toList' (i3:accumulator) next

tempLift :: Free BindingF () -> Free StatementF ()
tempLift x = liftF' (BindingStatement (head (toBindingList x)))
