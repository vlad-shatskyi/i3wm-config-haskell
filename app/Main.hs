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

moveTo w = do
  moveToWorkspace w
  focusWorkspace w
  exit

resizeProportionally gs = do
  resize gs Width resizeStep
  resize gs Height resizeStep
  lh MoveToCenter

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

  RaiseVolumeSym ==> exec "amixer -q sset Master 5%+ unmute && pkill -RTMIN+10 i3blocks"
  LowerVolumeSym ==> exec "amixer -q sset Master 5%- unmute && pkill -RTMIN+10 i3blocks"
  MuteSym ==> exec "amixer -q sset Master,0 toggle && pkill -RTMIN+10 i3blocks"

  BrightnessUpSym ==> exec "xbacklight -inc 10"
  BrightnessDownSym ==> exec "xbacklight -dec 10"

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

  [Mod4Sym, SpaceSym] ==> focusModeToggle
  [Mod4Sym, ShiftSym, SpaceSym] ==> floatingToggle

  Super Minus ==> toggleScratchpad
  SuperShift Minus ==> do
    stickyEnable
    moveToScratchpad

  SuperCtrl C ==> exec "clipmenu"

  Super T ==> ActionsWithCriteria terminal (lh ToggleScratchpad)
  Super O ==> exec "emacsclient -c -n ~/notes/notes.org"
  [Mod4Sym, EqualSym] ==> ActionsWithCriteria telegram (lh ToggleScratchpad)

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

  keyboardLayoutMode <- mode "Keyboard Layout Mode" $ do
    E ==>^ exec (setXkb "us")
    R ==>^ exec (setXkb "ru")
    U ==>^ exec (setXkb "ua")

  Super I ==> activateMode keyboardLayoutMode

  layoutMode <- mode "Layout Mode" $ do
    S ==>^ layoutStacking
    T ==>^ layoutTabbed
    V ==>^ layoutSplitHorizontally
    H ==>^ layoutSplitVertically

  i3ManagementMode <- mode "i3 Management Mode" $ do
    C ==>^ reloadWM
    R ==>^ restartWM
    E ==>^ exitWM
    W ==>^ exec "rofi -show window"

    L ==> activateMode layoutMode
  Super Tilde ==> activateMode i3ManagementMode

  resizeMode <- mode "Resize Mode" $ do
    W ==> resize Grow Width resizeStep
    N ==> resize Shrink Width resizeStep
    H ==> resize Grow Height resizeStep
    L ==> resize Shrink Height resizeStep

    Equal ==> resizeProportionally Grow
    Minus ==> resizeProportionally Shrink
  Super R ==> activateMode resizeMode


  moveMode <- mode "Move Mode" $ do
    H ==> moveLeft moveStep
    L ==> moveRight moveStep
    J ==> moveDown moveStep
    K ==> moveUp moveStep
    C ==>^ moveToCenter

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
      resizeTo dockedWindowWidth dockedWindowHeight
      moveToPosition (quot (screenWidth - dockedWindowWidth) 2) (screenHeight - dockedWindowHeight)
      lh FocusTiling

    R ==>^ do
      lh FocusFloating
      resizeTo (quot screenWidth 3 * 2) (quot screenHeight 10 * 9)
      lh MoveToCenter
  Super M ==> activateMode moveMode

main :: IO ()
main = iterM Languages.I3.interpretTopLevelF config
