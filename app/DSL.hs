module DSL where

import DataTypes.Key
import Languages.I3
import DataTypes.Other
import Control.Monad.Free
import Data.Maybe (mapMaybe)

data Op next = Op Statement next deriving (Functor)
type Config = Free Op

liftF' :: Statement -> Config ()
liftF' x = liftF $ Op x ()

liftF'' = (liftF' .)

actionList' :: [ActionCriteria] -> [Action] -> ActionList
actionList' cs xs = ActionList [ActionsWithCriteria cs xs]

action' :: [ActionCriteria] -> Action -> ActionList
action' cs x = actionList' cs [x]

exec x = liftF' (I3Action (toActionList (Exec x)))
execAlways = liftF' . ExecAlways
raw = liftF' . Raw
font = liftF'' . Font
bindsym k a= liftF' (BindSym k (toActionList a))

bindcode s a = liftF' (BindCode DontRelease (shortcut s) (toActionList a))
a ==> b = bindcode a b

bar = liftF' . Bar
hideEdgeBorders = liftF' HideEdgeBorders
forWindow criteria actions = liftF' (ForWindow (ActionsWithCriteria criteria actions))

mode shortcut name config = bindcode shortcut (ActivateMode modeName) >> liftF' (Mode modeName modeStatements)
  where modeName = ModeName name
        modeStatements = toList (bindsym [EscapeSym] exit >> config)

application :: [ActionCriteria] -> Config () -> Config ()
application criteria config = liftF' (List (mapMaybe addCriteria (toList config)))
  where addCriteria (BindCode r s (ActionList actionsWithCriteria)) = Just (BindCode r s (ActionList (map addCriteria' actionsWithCriteria)))
        addCriteria _ = Nothing
        addCriteria' (ActionsWithCriteria cs as) = ActionsWithCriteria (cs ++ criteria ++ [IsCurrent]) as

exit = ActivateMode (ModeName "default")

class ActionListConvertible x where
  toActionList :: x -> ActionList

instance ActionListConvertible Action where
  toActionList x = toActionList [x]

instance ActionListConvertible [Action] where
  toActionList xs = ActionList [ActionsWithCriteria [] xs]

instance ActionListConvertible ActionList where
  toActionList = id

toList :: Config a -> [Statement]
toList = reverse . toList' []
  where toList' accumulator (Pure _) = accumulator
        toList' accumulator (Free (Op i3 next)) = toList' (i3:accumulator) next

-- TODO: refactor.
flatten :: [Statement] -> [Statement]
flatten ss = modes
  where modes = foldl f [] ss
        f acc (Mode name css) = (Mode name (filter (not . isMode) css):acc) ++ flatten css
        f acc _ = acc
        isMode (Mode _ _) = True
        isMode _ = False
