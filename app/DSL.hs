module DSL where

import DataTypes.Key
import Languages.I3
import DataTypes.Other
import Control.Monad.Free

class Hoist f g where
  hoist :: (() -> f ()) -> Free g ()

instance Hoist TopLevelF TopLevelF where
  hoist fx = liftF $ fx ()

instance Hoist BindingF BindingF where
  hoist fx = liftF $ fx ()

instance Hoist ActionF ActionF where
  hoist fx = liftF $ fx ()

instance Hoist StatementF TopLevelF where
  hoist fx = liftF $ LL (fx ())

instance Hoist BindingF TopLevelF where
  hoist fx = liftF $ RR (fx ())

freeActionF :: Action -> Free ActionF ()
freeActionF = hoist . ActionF

exec x = freeActionF $ Exec x
moveToWorkspace x = freeActionF $ MoveToWorkspace x
focusWorkspace x = freeActionF $ FocusWorkspace x
moveToPosition x y = freeActionF $ MoveToPosition x y
resizeTo x y = freeActionF $ ResizeTo x y
activateMode id = freeActionF $ ActivateMode id
moveLeft x = freeActionF $ MoveLeft x
moveRight x = freeActionF $ MoveRight x
moveDown x = freeActionF $ MoveDown x
moveUp x = freeActionF $ MoveUp x
moveToCenter = freeActionF MoveToCenter
layoutStacking = freeActionF LayoutStacking
layoutTabbed = freeActionF LayoutTabbed
layoutSplitHorizontally = freeActionF LayoutSplitHorizontally
layoutSplitVertically = freeActionF LayoutSplitVertically
reloadWM = freeActionF ReloadWM
restartWM = freeActionF RestartWM
exitWM = freeActionF ExitWM
focusLeft = freeActionF FocusLeft
focusRight = freeActionF FocusRight
moveToScratchpad = freeActionF MoveToScratchpad
stickyEnable = freeActionF StickyEnable
floatingEnable = freeActionF FloatingEnable
fullscreenEnable = freeActionF FullscreenEnable
fullscreenToggle = freeActionF FullscreenToggle
splitToggle = freeActionF SplitToggle
focusModeToggle = freeActionF FocusModeToggle
floatingToggle = freeActionF FloatingToggle
toggleScratchpad = freeActionF ToggleScratchpad
closeWindow = freeActionF CloseWindow
focusFloating = freeActionF FocusFloating
focusTiling = freeActionF FocusTiling
resize gs wh x = freeActionF $ Resize gs wh x
exit = freeActionF $ ActivateMode (ModeIdentifier "default")

exec' x = hoist $ ExecStatement x
execAlways a = hoist $ ExecAlways a
raw a = hoist $ Raw a
font a b = hoist $ Font a b

s ==> a = hoist $ BindingF $ bind s a

-- Bind and exit mode.
s ==>^ a = s ==> ActionsWithCriteria criteria (actions >> exit)
  where (ActionsWithCriteria criteria actions) = addCriteria a

bar x = hoist $ Bar x
hideEdgeBorders _ = hoist HideEdgeBorders
forWindow criteria actions = hoist $ ForWindow (ActionsWithCriteria criteria actions)

mode name config = do
  hoist $ ModeDefinition identifier bindings
  return identifier
  where identifier = ModeIdentifier name
        bindings = do
          EscapeSym ==> exit
          Q ==> exit
          config

toBindingList :: Free BindingF a -> [Binding]
toBindingList = reverse . toList' []
  where toList' accumulator (Pure _) = accumulator
        toList' accumulator (Free (BindingF i3 next)) = toList' (i3:accumulator) next


class AddCriteria a where
  addCriteria :: a -> ActionsWithCriteria

instance AddCriteria ActionsWithCriteria where
  addCriteria = id

instance AddCriteria (Free ActionF ()) where
  addCriteria = ActionsWithCriteria []

class Bind k where
  bind :: (AddCriteria a) => k -> a -> Binding

instance Bind KeyName where
  bind k a = BindSym [k] (addCriteria a)

instance Bind [KeyName] where
  bind k a = BindSym k (addCriteria a)

instance Bind Key where
  bind k a = BindCode DontRelease (NoModifier k) (addCriteria a)

instance Bind Shortcut where
  bind k a = BindCode DontRelease k (addCriteria a)
