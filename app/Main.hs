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

  exec "google-chrome-unstable"
  exec "slack"
  exec "telegram-desktop"

  font ["pango", "monospace"] 8

  bar "i3blocks"
  hideEdgeBorders ()

  bindsym [RaiseVolumeSym] (Exec "amixer -q sset Master 5%+ unmute && pkill -RTMIN+10 i3blocks")
  bindsym [LowerVolumeSym] (Exec "amixer -q sset Master 5%- unmute && pkill -RTMIN+10 i3blocks")
  bindsym [MuteSym] (Exec "amixer -q sset Master,0 toggle && pkill -RTMIN+10 i3blocks")

  bindsym [BrightnessUpSym] (Exec "xbacklight -inc 10")
  bindsym [BrightnessDownSym] (Exec "xbacklight -dec 10")

  Super Return ==> Exec "konsole"
  Super W ==> CloseWindow
  Super Slash ==> Exec "rofi -show drun"

  forWindow chrome [MoveToWorkspace W1]
  forWindow rubymine [MoveToWorkspace W2]
  forWindow idea [MoveToWorkspace W2]
  forWindow slack [MoveToWorkspace W4]
  forWindow telegram [MoveToScratchpad, StickyEnable]
  forWindow fileManager [FloatingEnable, MoveToPosition 0 0]
  forWindow videoPlayer [FullscreenEnable]

  bindsym [Mod4Sym, SpaceSym] FocusModeToggle
  bindsym [Mod4Sym, ShiftSym, SpaceSym] FloatingToggle

  Super Minus ==> ToggleScratchpad
  SuperShift Minus ==> [StickyEnable, MoveToScratchpad]

  SuperCtrl C ==> Exec "clipmenu"

  Super T ==> ActionsWithCriteria terminal [ToggleScratchpad]
  Super O ==> Exec "emacsclient -c -n ~/notes/notes.org"
  bindsym [Mod4Sym, EqualSym] (ActionsWithCriteria telegram [ToggleScratchpad])

  Super LeftBracket ==> FocusLeft
  Super RightBracket ==> FocusRight
  Super F ==> FullscreenToggle
  Super H ==> SplitToggle

  Super J ==> FocusWorkspace W1
  Super K ==> FocusWorkspace W2
  Super L ==> FocusWorkspace W3
  Super Semicolon ==> FocusWorkspace W4
  Super Quote ==> FocusWorkspace W9

  Super N1 ==> FocusWorkspace W1
  Super N2 ==> FocusWorkspace W2
  Super N3 ==> FocusWorkspace W3
  Super N4 ==> FocusWorkspace W4
  Super N5 ==> FocusWorkspace W5
  Super N6 ==> FocusWorkspace W6
  Super N7 ==> FocusWorkspace W7
  Super N8 ==> FocusWorkspace W8
  Super N9 ==> FocusWorkspace W9
  Super N0 ==> FocusWorkspace W0

  Super I ==> ActivateMode (ModeName "Keyboard Layout Mode")
  mode "Keyboard Layout Mode" $ do
    E ==> [Exec (setXkb "us"), exit]
    R ==> [Exec (setXkb "ru"), exit]
    U ==> [Exec (setXkb "ua"), exit]

  Super Tilde ==> ActivateMode (ModeName "i3 Management Mode")
  mode "i3 Management Mode" $ do
    C ==> [ReloadWM, exit]
    R ==> [RestartWM, exit]
    W ==> [Exec "rofi -show window", exit]

    L ==> ActivateMode (ModeName "Layout Mode")

  mode "Layout Mode" $ do
    S ==> [LayoutStacking, exit]
    T ==> [LayoutTabbed, exit]
    V ==> [LayoutSplitHorizontally, exit]
    H ==> [LayoutSplitVertically, exit]

  Super R ==> ActivateMode (ModeName "Resize Mode")
  mode "Resize Mode" $ do
    W ==> Resize Grow Width resizeStep
    N ==> Resize Shrink Width resizeStep
    H ==> Resize Grow Height resizeStep
    L ==> Resize Shrink Height resizeStep
    Equal ==> [Resize Grow Width resizeStep, Resize Grow Height resizeStep, MoveToCenter]
    Minus ==> [Resize Shrink Width resizeStep, Resize Shrink Height resizeStep, MoveToCenter]

  Super M ==> ActivateMode (ModeName "Move Mode")
  mode "Move Mode" $ do
    H ==> MoveLeft moveStep
    L ==> MoveRight moveStep
    J ==> MoveDown moveStep
    K ==> MoveUp moveStep
    C ==> [MoveToCenter, exit]

    N1 ==> [MoveToWorkspace W1, FocusWorkspace W1, exit]
    N2 ==> [MoveToWorkspace W2, FocusWorkspace W2, exit]
    N3 ==> [MoveToWorkspace W3, FocusWorkspace W3, exit]
    N4 ==> [MoveToWorkspace W4, FocusWorkspace W4, exit]
    N5 ==> [MoveToWorkspace W5, FocusWorkspace W5, exit]
    N6 ==> [MoveToWorkspace W6, FocusWorkspace W6, exit]
    N7 ==> [MoveToWorkspace W7, FocusWorkspace W7, exit]
    N8 ==> [MoveToWorkspace W8, FocusWorkspace W8, exit]
    N9 ==> [MoveToWorkspace W9, FocusWorkspace W9, exit]
    N0 ==> [MoveToWorkspace W0, FocusWorkspace W0, exit]

    D ==> [ ResizeTo dockedWindowWidth dockedWindowHeight
          , MoveToPosition (quot (screenWidth - dockedWindowWidth) 2) (screenHeight - dockedWindowHeight)
          , FocusTiling
          , exit
          ]

    R ==> [ FocusFloating
          , ResizeTo (quot screenWidth 3 * 2) (quot screenHeight 10 * 9)
          , MoveToCenter
          , exit
          ]

main :: IO ()
main = iterM Languages.I3.interpretTopLevelF config
