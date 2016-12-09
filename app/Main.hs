module Main where

import Languages.I3
import DataTypes.Key
import DataTypes.Other
import DSL
import Control.Monad.Free

focusedWindowClass = "i3-msg -t get_tree | jq 'recurse(.nodes[], .floating_nodes[]) | select(.focused == true) | .window_properties | .class'"

chrome = [Instance "google-chrome-unstable"]
rubymine = [Class "jetbrains-rubymine"]
idea = [Class "jetbrains-idea-ce"]
fileManager = [Class "Nautilus"]
videoPlayer = [Class "mpv"]
slack = [Instance "slack"]
telegram = [Title "Telegram"]
terminal = [Instance "konsole", IsFloating]
emacs = [Instance "emacs"]

setLayout layout = "setxkbmap " ++ layout ++ " && pkill -RTMIN+11 i3blocks"
moveStep = 50
resizeStep = 50
screenWidth = 3840
screenHeight = 2160

dockedWindowWidth = quot screenWidth 3 * 2
dockedWindowHeight = 140

moveTo w = do
  lift (MoveToWorkspace w)
  lift (FocusWorkspace w)
  exit

resizeProportionally gs = do
  lift (Resize gs Width resizeStep)
  lift (Resize gs Height resizeStep)
  lift MoveToCenter

config :: Free TopLevelF ()
config = do
  execAlways "xinput set-prop 12 281 1" -- Enable Tapping.
  execAlways "xinput set-prop 12 283 0" -- Disable Tapping Drag.
  execAlways "xinput set-prop 12 289 0.85" -- Increase Accel Speed.
  execAlways "xinput set-prop 12 291 1" -- Enable natural scroll.
  execAlways "setxkbmap -option altwin:swap_alt_win"
  execAlways "setxkbmap -option ctrl:nocaps"

  execNoStartupId "/usr/lib/gnome-settings-daemon/gnome-settings-daemon"
  execNoStartupId "dunst"
  execNoStartupId "clipmenud"
  execNoStartupId "emacs --daemon"

  raw "floating_modifier Mod4"
  raw $ "floating_maximum_size " ++ show screenWidth ++ " x " ++ show screenHeight
  raw "focus_follows_mouse no"

  exec' "google-chrome-unstable"
  exec' "slack"
  exec' "telegram-desktop"

  font ["pango", "monospace"] 8

  bar "i3blocks"
  hideEdgeBorders ()

  RaiseVolumeSym ==> lift (Exec "amixer -q sset Master 5%+ unmute && pkill -RTMIN+10 i3blocks")
  LowerVolumeSym ==> lift (Exec "amixer -q sset Master 5%- unmute && pkill -RTMIN+10 i3blocks")
  MuteSym ==> lift (Exec "amixer -q sset Master,0 toggle && pkill -RTMIN+10 i3blocks")

  BrightnessUpSym ==> lift (Exec "xbacklight -inc 10")
  BrightnessDownSym ==> lift (Exec "xbacklight -dec 10")

  Super Return ==> lift (Exec "konsole")
  Super W ==> lift CloseWindow
  Super Slash ==> lift (Exec "rofi -show drun")

  forWindow chrome (lift (MoveToWorkspace W1))
  forWindow rubymine (lift (MoveToWorkspace W2))
  forWindow idea (lift (MoveToWorkspace W2))
  forWindow slack (lift (MoveToWorkspace W4))
  forWindow telegram $ do
    lift MoveToScratchpad
    lift StickyEnable
  forWindow emacs (lift FloatingEnable)

  forWindow fileManager $ do
    lift FloatingEnable
    lift (MoveToPosition 0 0)

  forWindow videoPlayer (lift FullscreenEnable)

  [Mod4Sym, SpaceSym] ==> lift FocusModeToggle
  [Mod4Sym, ShiftSym, SpaceSym] ==> lift FloatingToggle

  Super Minus ==> lift ToggleScratchpad
  SuperShift Minus ==> do
    lift StickyEnable
    lift MoveToScratchpad

  SuperCtrl C ==> lift (Exec "clipmenu")

  Super T ==> ActionsWithCriteria terminal (lift ToggleScratchpad)
  Super N ==> do
    lift (Exec "emacsclient -c -n ~/notes/notes.org")
    lift FocusFloating
  [Mod4Sym, EqualSym] ==> ActionsWithCriteria telegram (lift ToggleScratchpad)

  Super LeftBracket ==> lift FocusLeft
  Super RightBracket ==> lift FocusRight
  Super F ==> lift FullscreenToggle
  Super H ==> lift SplitToggle

  Super J ==> lift (FocusWorkspace W1)
  Super K ==> lift (FocusWorkspace W2)
  Super L ==> lift (FocusWorkspace W3)
  Super Semicolon ==> lift (FocusWorkspace W4)
  Super Quote ==> lift (FocusWorkspace W9)

  Super N1 ==> lift (FocusWorkspace W1)
  Super N2 ==> lift (FocusWorkspace W2)
  Super N3 ==> lift (FocusWorkspace W3)
  Super N4 ==> lift (FocusWorkspace W4)
  Super N5 ==> lift (FocusWorkspace W5)
  Super N6 ==> lift (FocusWorkspace W6)
  Super N7 ==> lift (FocusWorkspace W7)
  Super N8 ==> lift (FocusWorkspace W8)
  Super N9 ==> lift (FocusWorkspace W9)
  Super N0 ==> lift (FocusWorkspace W0)

  keyboardLayoutMode <- mode "Keyboard Layout Mode" $ do
    E ==>^ lift (Exec (setLayout "us"))
    Super I ==>^ lift (Exec (setLayout "us"))
    R ==>^ lift (Exec (setLayout "ru"))
    U ==>^ lift (Exec (setLayout "ua"))

  Super I ==> lift (ActivateMode keyboardLayoutMode)

  layoutMode <- mode "Layout Mode" $ do
    S ==>^ lift LayoutStacking
    T ==>^ lift LayoutTabbed
    V ==>^ lift LayoutSplitHorizontally
    H ==>^ lift LayoutSplitVertically

  i3ManagementMode <- mode "i3 Management Mode" $ do
    C ==>^ lift ReloadWM
    R ==>^ lift RestartWM
    E ==>^ lift ExitWM
    W ==>^ lift (Exec "rofi -show window")

    L ==> lift (ActivateMode layoutMode)
  Super Tilde ==> lift (ActivateMode i3ManagementMode)

  resizeMode <- mode "Resize Mode" $ do
    W ==> lift (Resize Grow Width resizeStep)
    N ==> lift (Resize Shrink Width resizeStep)
    H ==> lift (Resize Grow Height resizeStep)
    L ==> lift (Resize Shrink Height resizeStep)

    Equal ==> resizeProportionally Grow
    Minus ==> resizeProportionally Shrink
  Super R ==> lift (ActivateMode resizeMode)


  moveMode <- mode "Move Mode" $ do
    H ==> lift (MoveLeft moveStep)
    L ==> lift (MoveRight moveStep)
    J ==> lift (MoveDown moveStep)
    K ==> lift (MoveUp moveStep)
    C ==>^ lift MoveToCenter

    N1 ==> moveTo W1
    N2 ==> moveTo W2
    N3 ==> moveTo W3
    N4 ==> moveTo W4
    N5 ==> moveTo W5
    N6 ==> moveTo W6
    N7 ==> moveTo W7
    N8 ==> moveTo W8
    N9 ==> moveTo W9
    N0 ==> moveTo W0

    D ==>^ do
      lift (ResizeTo dockedWindowWidth dockedWindowHeight)
      lift (MoveToPosition (quot (screenWidth - dockedWindowWidth) 2) (screenHeight - dockedWindowHeight))
      lift FocusTiling

    R ==>^ do
      lift FocusFloating
      lift (ResizeTo (quot screenWidth 3 * 2) (quot screenHeight 10 * 9))
      lift MoveToCenter
  Super M ==> lift (ActivateMode moveMode)

  Super A ==> lift (Exec ("i3-msg mode $(" ++ focusedWindowClass ++ ")"))

  _ <- mode "jetbrains-idea-ce" $ do
    N ==>^ lift (Exec "sleep 0.1 && xdotool key --clearmodifiers ctrl+shift+n")
    F ==>^ lift (Exec "sleep 0.1 && xdotool key --clearmodifiers ctrl+shift+f")
    R ==>^ lift (Exec "sleep 0.1 && xdotool key --clearmodifiers Escape && xdotool key --clearmodifiers alt+1 && xdotool key --clearmodifiers shift+F6")

  return ()

main :: IO ()
main = iterM Languages.I3.interpretTopLevelF config
