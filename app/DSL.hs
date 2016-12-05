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

instance LH ActionF ActionF where
  lh fx = liftF $ fx ()

instance LH StatementF TopLevelF where
  lh fx = liftF $ LL (fx ())

instance LH BindingF TopLevelF where
  lh fx = liftF $ RR (fx ())

exec :: String -> Free ActionF ()
exec x = lh $ ActionF $ Exec x

moveToWorkspace :: WorkspaceNumber -> Free ActionF ()
moveToWorkspace x = lh $ ActionF $ MoveToWorkspace x

focusWorkspace :: WorkspaceNumber -> Free ActionF ()
focusWorkspace x = lh $ ActionF $ FocusWorkspace x

moveToPosition :: Int -> Int -> Free ActionF ()
moveToPosition x y = lh $ ActionF $ MoveToPosition x y

resizeTo :: Int -> Int -> Free ActionF ()
resizeTo x y = lh $ ActionF $ ResizeTo x y

activateMode :: ModeIdentifier -> Free ActionF ()
activateMode id = lh $ ActionF $ ActivateMode id

moveLeft :: Int -> Free ActionF ()
moveLeft x = lh $ ActionF $ MoveLeft x

moveRight :: Int -> Free ActionF ()
moveRight x = lh $ ActionF $ MoveRight x

moveDown :: Int -> Free ActionF ()
moveDown x = lh $ ActionF $ MoveDown x

moveUp :: Int -> Free ActionF ()
moveUp x = lh $ ActionF $ MoveUp x

moveToCenter :: Free ActionF ()
moveToCenter = lh $ ActionF MoveToCenter

layoutStacking :: Free ActionF ()
layoutStacking = lh $ ActionF LayoutStacking

layoutTabbed :: Free ActionF ()
layoutTabbed = lh $ ActionF LayoutTabbed

layoutSplitHorizontally :: Free ActionF ()
layoutSplitHorizontally = lh $ ActionF LayoutSplitHorizontally

layoutSplitVertically :: Free ActionF ()
layoutSplitVertically = lh $ ActionF LayoutSplitVertically

reloadWM :: Free ActionF ()
reloadWM = lh $ ActionF ReloadWM

restartWM :: Free ActionF ()
restartWM = lh $ ActionF RestartWM

exitWM :: Free ActionF ()
exitWM = lh $ ActionF ExitWM

focusLeft :: Free ActionF ()
focusLeft = lh $ ActionF FocusLeft

focusRight :: Free ActionF ()
focusRight = lh $ ActionF FocusRight

moveToScratchpad :: Free ActionF ()
moveToScratchpad = lh $ ActionF MoveToScratchpad

stickyEnable :: Free ActionF ()
stickyEnable = lh $ ActionF StickyEnable

floatingEnable :: Free ActionF ()
floatingEnable = lh $ ActionF FloatingEnable

fullscreenEnable :: Free ActionF ()
fullscreenEnable = lh $ ActionF FullscreenEnable

fullscreenToggle :: Free ActionF ()
fullscreenToggle = lh $ ActionF FullscreenToggle

splitToggle :: Free ActionF ()
splitToggle = lh $ ActionF SplitToggle

focusModeToggle :: Free ActionF ()
focusModeToggle = lh $ ActionF FocusModeToggle

floatingToggle :: Free ActionF ()
floatingToggle = lh $ ActionF FloatingToggle

toggleScratchpad :: Free ActionF ()
toggleScratchpad = lh $ ActionF ToggleScratchpad

closeWindow :: Free ActionF ()
closeWindow = lh $ ActionF CloseWindow

focusFloating :: Free ActionF ()
focusFloating = lh $ ActionF FocusFloating

focusTiling :: Free ActionF ()
focusTiling = lh $ ActionF FocusTiling

resize :: GrowOrShrink -> WidthOrHeight -> Int -> Free ActionF ()
resize gs wh x = lh $ ActionF $ Resize gs wh x

exit :: Free ActionF ()
exit = lh $ ActionF $ ActivateMode (ModeIdentifier "default")

exec' x = lh $ ExecStatement x
execAlways a = lh $ ExecAlways a
raw a = lh $ Raw a
font a b = lh $ Font a b

s ==> a = lh $ BindingF $ bind s a

-- Bind and exit mode.
s ==>^ a = s ==> ActionsWithCriteria criteria (actions >> exit)
  where (ActionsWithCriteria criteria actions) = addCriteria a

bar x = lh $ Bar x
hideEdgeBorders _ = lh HideEdgeBorders
forWindow criteria actions = lh $ ForWindow (ActionsWithCriteria criteria actions)

mode name config = do
  lh $ ModeDefinition identifier bindings
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
