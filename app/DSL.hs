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

instance LH LanguageF TopLevelF where
  lh fx = liftF $ LL (fx ())

instance LH BindingF TopLevelF where
  lh fx = liftF $ RR (fx ())

exec :: String -> Free ActionF ()
exec x = lh $ Exec x

moveToWorkspace :: WorkspaceNumber -> Free ActionF ()
moveToWorkspace x = lh $ MoveToWorkspace x

focusWorkspace :: WorkspaceNumber -> Free ActionF ()
focusWorkspace x = lh $ FocusWorkspace x

moveToPosition :: Int -> Int -> Free ActionF ()
moveToPosition x y = lh $ MoveToPosition x y

resizeTo :: Int -> Int -> Free ActionF ()
resizeTo x y = lh $ ResizeTo x y

activateMode :: String -> Free ActionF ()
activateMode name = lh $ ActivateMode (ModeIdentifier name)

moveLeft :: Int -> Free ActionF ()
moveLeft x = lh $ MoveLeft x

moveRight :: Int -> Free ActionF ()
moveRight x = lh $ MoveRight x

moveDown :: Int -> Free ActionF ()
moveDown x = lh $ MoveDown x

moveUp :: Int -> Free ActionF ()
moveUp x = lh $ MoveUp x

focusLeft :: Free ActionF ()
focusLeft = lh FocusLeft

focusRight :: Free ActionF ()
focusRight = lh FocusRight

moveToScratchpad :: Free ActionF ()
moveToScratchpad = lh MoveToScratchpad

stickyEnable :: Free ActionF ()
stickyEnable = lh StickyEnable

floatingEnable :: Free ActionF ()
floatingEnable = lh FloatingEnable

fullscreenEnable :: Free ActionF ()
fullscreenEnable = lh FullscreenEnable

fullscreenToggle :: Free ActionF ()
fullscreenToggle = lh FullscreenToggle

splitToggle :: Free ActionF ()
splitToggle = lh SplitToggle

focusModeToggle :: Free ActionF ()
focusModeToggle = lh FocusModeToggle

floatingToggle :: Free ActionF ()
floatingToggle = lh FloatingToggle

toggleScratchpad :: Free ActionF ()
toggleScratchpad = lh ToggleScratchpad

closeWindow :: Free ActionF ()
closeWindow = lh CloseWindow

resize :: GrowOrShrink -> WidthOrHeight -> Int -> Free ActionF ()
resize gs wh x = lh $ Resize gs wh x

exit :: Free ActionF ()
exit = lh $ ActivateMode (ModeIdentifier "default")

exec' x = lh $ ExecStatement x
execAlways a = lh $ ExecAlways a
raw a = lh $ Raw a
font a b = lh $ Font a b

bindsym k a = lh $ BindingF (BindSym k (addCriteria a))
bindcode s a = lh $ BindingF (BindCode DontRelease (shortcut s) (addCriteria a))
s ==> a = bindcode s a

bar x = lh $ Bar x
hideEdgeBorders _ = lh HideEdgeBorders
forWindow criteria actions = lh $ ForWindow (ActionsWithCriteria criteria actions)

mode name config = lh $ ModeDefinition modeName bindings
  where modeName = ModeIdentifier name
        bindings = bindsym [EscapeSym] exit >> bindcode Q exit >> config

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
