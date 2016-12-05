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

exec :: String -> Free ActionF ()
exec x = hoist $ ActionF $ Exec x

moveToWorkspace :: WorkspaceNumber -> Free ActionF ()
moveToWorkspace x = hoist $ ActionF $ MoveToWorkspace x

focusWorkspace :: WorkspaceNumber -> Free ActionF ()
focusWorkspace x = hoist $ ActionF $ FocusWorkspace x

moveToPosition :: Int -> Int -> Free ActionF ()
moveToPosition x y = hoist $ ActionF $ MoveToPosition x y

resizeTo :: Int -> Int -> Free ActionF ()
resizeTo x y = hoist $ ActionF $ ResizeTo x y

activateMode :: ModeIdentifier -> Free ActionF ()
activateMode id = hoist $ ActionF $ ActivateMode id

moveLeft :: Int -> Free ActionF ()
moveLeft x = hoist $ ActionF $ MoveLeft x

moveRight :: Int -> Free ActionF ()
moveRight x = hoist $ ActionF $ MoveRight x

moveDown :: Int -> Free ActionF ()
moveDown x = hoist $ ActionF $ MoveDown x

moveUp :: Int -> Free ActionF ()
moveUp x = hoist $ ActionF $ MoveUp x

moveToCenter :: Free ActionF ()
moveToCenter = hoist $ ActionF MoveToCenter

layoutStacking :: Free ActionF ()
layoutStacking = hoist $ ActionF LayoutStacking

layoutTabbed :: Free ActionF ()
layoutTabbed = hoist $ ActionF LayoutTabbed

layoutSplitHorizontally :: Free ActionF ()
layoutSplitHorizontally = hoist $ ActionF LayoutSplitHorizontally

layoutSplitVertically :: Free ActionF ()
layoutSplitVertically = hoist $ ActionF LayoutSplitVertically

reloadWM :: Free ActionF ()
reloadWM = hoist $ ActionF ReloadWM

restartWM :: Free ActionF ()
restartWM = hoist $ ActionF RestartWM

exitWM :: Free ActionF ()
exitWM = hoist $ ActionF ExitWM

focusLeft :: Free ActionF ()
focusLeft = hoist $ ActionF FocusLeft

focusRight :: Free ActionF ()
focusRight = hoist $ ActionF FocusRight

moveToScratchpad :: Free ActionF ()
moveToScratchpad = hoist $ ActionF MoveToScratchpad

stickyEnable :: Free ActionF ()
stickyEnable = hoist $ ActionF StickyEnable

floatingEnable :: Free ActionF ()
floatingEnable = hoist $ ActionF FloatingEnable

fullscreenEnable :: Free ActionF ()
fullscreenEnable = hoist $ ActionF FullscreenEnable

fullscreenToggle :: Free ActionF ()
fullscreenToggle = hoist $ ActionF FullscreenToggle

splitToggle :: Free ActionF ()
splitToggle = hoist $ ActionF SplitToggle

focusModeToggle :: Free ActionF ()
focusModeToggle = hoist $ ActionF FocusModeToggle

floatingToggle :: Free ActionF ()
floatingToggle = hoist $ ActionF FloatingToggle

toggleScratchpad :: Free ActionF ()
toggleScratchpad = hoist $ ActionF ToggleScratchpad

closeWindow :: Free ActionF ()
closeWindow = hoist $ ActionF CloseWindow

focusFloating :: Free ActionF ()
focusFloating = hoist $ ActionF FocusFloating

focusTiling :: Free ActionF ()
focusTiling = hoist $ ActionF FocusTiling

resize :: GrowOrShrink -> WidthOrHeight -> Int -> Free ActionF ()
resize gs wh x = hoist $ ActionF $ Resize gs wh x

exit :: Free ActionF ()
exit = hoist $ ActionF $ ActivateMode (ModeIdentifier "default")

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
