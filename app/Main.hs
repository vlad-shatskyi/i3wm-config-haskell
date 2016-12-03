module Main where

import Languages.I3
import DataTypes.Key
import DataTypes.Other
import DSL
import Control.Monad.Free

chrome = [Instance "google-chrome-unstable"]
rubymine = [Class "jetbrains-rubymine"]
idea = [Class "jetbrains-idea-ce"]
fileManager = [Class "Nautilus"]
videoPlayer = [Class "mpv"]
slack = [Instance "slack"]
telegram = [Title "Telegram"]
terminal = [Instance "konsole", IsFloating]

setXkb layout = "setxkbmap " ++ layout ++ " && pkill -RTMIN+11 i3blocks"
moveStep = 50
resizeStep = 50
screenWidth = 3840
screenHeight = 2160

dockedWindowWidth = quot screenWidth 3 * 2
dockedWindowHeight = 140

config :: Free TopLevelF ()
config = do
  execAlways "xinput set-prop 12 281 1" -- Enable Tapping.
  execAlways "xinput set-prop 12 283 0" -- Disable Tapping Drag.
  execAlways "xinput set-prop 12 289 0.85" -- Increase Accel Speed.
  execAlways "xinput set-prop 12 291 1" -- Enable natural scroll.
  execAlways "setxkbmap -option altwin:swap_alt_win"
  execAlways "setxkbmap -option ctrl:nocaps"

  raw "exec --no-startup-id /usr/lib/gnome-settings-daemon/gnome-settings-daemon"
  raw "exec --no-startup-id dunst"
  raw "exec --no-startup-id clipmenud"
  raw "floating_modifier Mod4"
  raw $ "floating_maximum_size " ++ show screenWidth ++ " x " ++ show screenHeight
  raw "focus_follows_mouse no"

  exec' "google-chrome-unstable"
  exec' "slack"
  exec' "telegram-desktop"

  font ["pango", "monospace"] 8

  bar "i3blocks"
  hideEdgeBorders ()

  bindsym [RaiseVolumeSym] (exec "amixer -q sset Master 5%+ unmute && pkill -RTMIN+10 i3blocks")
  bindsym [LowerVolumeSym] (exec "amixer -q sset Master 5%- unmute && pkill -RTMIN+10 i3blocks")
  bindsym [MuteSym] (exec "amixer -q sset Master,0 toggle && pkill -RTMIN+10 i3blocks")

  bindsym [BrightnessUpSym] (exec "xbacklight -inc 10")
  bindsym [BrightnessDownSym] (exec "xbacklight -dec 10")

  Super Return ==> exec "konsole"
  Super W ==> closeWindow
  Super Slash ==> exec "rofi -show drun"

  forWindow chrome (moveToWorkspace W1)
  forWindow rubymine (moveToWorkspace W2)
  forWindow idea (moveToWorkspace W2)
  forWindow slack (moveToWorkspace W4)
  forWindow telegram $ do
    moveToScratchpad
    stickyEnable

  forWindow fileManager $ do
    floatingEnable
    moveToPosition 0 0

  forWindow videoPlayer fullscreenEnable

  bindsym [Mod4Sym, SpaceSym] focusModeToggle
  bindsym [Mod4Sym, ShiftSym, SpaceSym] floatingToggle

  Super Minus ==> toggleScratchpad
  SuperShift Minus ==> do
    stickyEnable
    moveToScratchpad

  SuperCtrl C ==> exec "clipmenu"

  Super T ==> ActionsWithCriteria terminal (lh ToggleScratchpad)
  Super O ==> exec "emacsclient -c -n ~/notes/notes.org"
  bindsym [Mod4Sym, EqualSym] (ActionsWithCriteria telegram (lh ToggleScratchpad))

  Super LeftBracket ==> focusLeft
  Super RightBracket ==> focusRight
  Super F ==> fullscreenToggle
  Super H ==> splitToggle

  Super J ==> focusWorkspace W1
  Super K ==> focusWorkspace W2
  Super L ==> focusWorkspace W3
  Super Semicolon ==> focusWorkspace W4
  Super Quote ==> focusWorkspace W9

  Super N1 ==> focusWorkspace W1
  Super N2 ==> focusWorkspace W2
  Super N3 ==> focusWorkspace W3
  Super N4 ==> focusWorkspace W4
  Super N5 ==> focusWorkspace W5
  Super N6 ==> focusWorkspace W6
  Super N7 ==> focusWorkspace W7
  Super N8 ==> focusWorkspace W8
  Super N9 ==> focusWorkspace W9
  Super N0 ==> focusWorkspace W0

  Super I ==> activateMode "Keyboard Layout Mode"
  mode "Keyboard Layout Mode" $ do
    E ==> do
      exec (setXkb "us")
      exit

    R ==> do
      exec (setXkb "ru")
      exit

    U ==> do
      exec (setXkb "ua")
      exit

  Super Tilde ==> activateMode "i3 Management Mode"
  mode "i3 Management Mode" $ do
    C ==> do
      lh ReloadWM
      exit

    R ==> do
      lh RestartWM
      exit

    W ==> do
      exec "rofi -show window"
      exit

    L ==> activateMode "Layout Mode"

  mode "Layout Mode" $ do
    S ==> do
      lh LayoutStacking
      exit

    T ==> do
      lh LayoutTabbed
      exit

    V ==> do
      lh LayoutSplitHorizontally
      exit

    H ==> do
      lh LayoutSplitVertically
      exit

  Super R ==> activateMode "Resize Mode"

  mode "Resize Mode" $ do
    W ==> resize Grow Width resizeStep
    N ==> resize Shrink Width resizeStep
    H ==> resize Grow Height resizeStep
    L ==> resize Shrink Height resizeStep
    Equal ==> do
      resize Grow Width resizeStep
      resize Grow Height resizeStep
      lh MoveToCenter

    Minus ==> do
      resize Shrink Width resizeStep
      resize Shrink Height resizeStep
      lh MoveToCenter

  Super M ==> activateMode "Move Mode"
  mode "Move Mode" $ do
    H ==> moveLeft moveStep
    L ==> moveRight moveStep
    J ==> moveDown moveStep
    K ==> moveUp moveStep
    C ==> do
      lh MoveToCenter
      exit

    N1 ==> do
      moveToWorkspace W1
      focusWorkspace W1
      exit

    N2 ==> do
      moveToWorkspace W2
      focusWorkspace W2
      exit

    N3 ==> do
      moveToWorkspace W3
      focusWorkspace W3
      exit

    N4 ==> do
      moveToWorkspace W4
      focusWorkspace W4
      exit

    N5 ==> do
      moveToWorkspace W5
      focusWorkspace W5
      exit

    N6 ==> do
      moveToWorkspace W6
      focusWorkspace W6
      exit

    N7 ==> do
      moveToWorkspace W7
      focusWorkspace W7
      exit

    N8 ==> do
      moveToWorkspace W8
      focusWorkspace W8
      exit

    N9 ==> do
      moveToWorkspace W9
      focusWorkspace W9
      exit

    N0 ==> do
      moveToWorkspace W0
      focusWorkspace W0
      exit

    D ==> do
      resizeTo dockedWindowWidth dockedWindowHeight
      moveToPosition (quot (screenWidth - dockedWindowWidth) 2) (screenHeight - dockedWindowHeight)
      lh FocusTiling
      exit

    R ==> do
      lh FocusFloating
      resizeTo (quot screenWidth 3 * 2) (quot screenHeight 10 * 9)
      lh MoveToCenter
      exit

main :: IO ()
main = iterM Languages.I3.interpretTopLevelF config
