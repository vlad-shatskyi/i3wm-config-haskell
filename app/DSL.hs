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

bindsym k a = hoistFree hoist $ liftF $ BindingF (BindSym k (toActionList a)) ()
bindcode s a = hoistFree hoist $ liftF $ BindingF (BindCode DontRelease (shortcut s) (toActionList a)) ()
a ==> b = bindcode a b

bar = liftF' . Bar
hideEdgeBorders = liftF' HideEdgeBorders
forWindow criteria actions = liftF' (ForWindow (ActionsWithCriteria criteria actions))

mode name config = liftF' (ModeDefinition modeName modeStatements)
  where modeName = ModeName name
        modeStatements = toBindingList (bindsym [EscapeSym] exit >> config)

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

class (Functor g) => Hoistable f g where
  hoist :: f b -> g b

instance (Functor f) => Hoistable f f where
  hoist = id

instance Hoistable BindingF StatementF where
  hoist (BindingF binding next) = StatementF (BindingStatement binding) next

interpretStatementF (StatementF statement next) = print statement >> next
