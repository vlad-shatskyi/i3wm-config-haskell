module DSL where

import Key
import Action
import Control.Monad.Free

data Op next = Op I3ConfigStatement next deriving (Functor)
type Config = Free Op

liftF' :: I3ConfigStatement -> Config ()
liftF' x = liftF $ Op x ()

liftF'' = (liftF' .)

actionList' :: [ActionCriteria] -> [Action] -> ActionList
actionList' cs xs = ActionList [ActionsWithCriteria cs xs]

action' :: [ActionCriteria] -> Action -> ActionList
action' cs x = actionList' cs [x]

focus = FocusAction BasedOnCriteriaFocusActionTarget

exec x = liftF' (I3Action (toActionList (ExecAction x)))
execAlways = liftF' . ExecAlways
font = liftF'' . Font
bindsym k a= liftF' (BindSym k (toActionList a))

bindcode s a = liftF' (BindCode (shortcut s) (toActionList a))
a ==> b = bindcode a b

bar = liftF' . Bar
hideEdgeBorders = liftF' HideEdgeBorders
forWindow criteria actions = liftF' (ForWindow (ActionsWithCriteria criteria actions))
mode shortcut name config = bindcode shortcut (ModeAction modeName) >> liftF' (Mode modeName modeStatements)
  where modeName = ModeName name
        modeStatements = toList (bindsym [EscapeSym] exit >> config)
exit = ModeAction (ModeName "default")

class ActionListConvertible x where
  toActionList :: x -> ActionList

instance ActionListConvertible Action where
  toActionList x = toActionList [x]

instance ActionListConvertible [Action] where
  toActionList xs = ActionList [ActionsWithCriteria [] xs]

instance ActionListConvertible ActionList where
  toActionList = id

toList :: Config a -> [I3ConfigStatement]
toList = reverse . toList' []
  where toList' accumulator (Pure _) = accumulator
        toList' accumulator (Free (Op i3 next)) = toList' (i3:accumulator) next

-- TODO: refactor.
flatten :: [I3ConfigStatement] -> [I3ConfigStatement]
flatten ss = modes
  where modes = foldl f [] ss
        f acc (Mode name css) = (Mode name (filter (not . isMode) css):acc) ++ flatten css
        f acc _ = acc
        isMode (Mode _ _) = True
        isMode _ = False
