module Main where

import Key
import Action
import DSL

chrome = [Instance "google-chrome-unstable"]
rubymine = [Class "jetbrains-rubymine"]
slack = [Instance "slack"]
telegram = [Title "Telegram"]
terminal = [Instance "urxvt", IsFloating]

setXkb layout = "setxkbmap " ++ layout ++ " && pkill -RTMIN+11 i3blocks"

config :: [I3ConfigStatement]
config = toList $ do
  execAlways "xinput set-prop 12 281 1" -- Enable Tapping.
  execAlways "xinput set-prop 12 283 0" -- Disable Tapping Drag.
  execAlways "xinput set-prop 12 289 0.85" -- Increase Accel Speed.
  execAlways "xinput set-prop 12 291 1" -- Enable natural scroll.
  execAlways "setxkbmap -option altwin:swap_alt_win"
  execAlways "setxkbmap -option ctrl:nocaps"

  raw "exec --no-startup-id dunst"
  raw "floating_modifier Mod4"

  exec "google-chrome-unstable"
  exec "slack"
  exec "telegram-desktop"

  font ["pango", "monospace"] 8

  bar "i3blocks"
  hideEdgeBorders

  bindsym [RaiseVolumeSym] (ExecAction "amixer -q sset Master 5%+ unmute && pkill -RTMIN+10 i3blocks")
  bindsym [LowerVolumeSym] (ExecAction "amixer -q sset Master 5%- unmute && pkill -RTMIN+10 i3blocks")
  bindsym [MuteSym] (ExecAction "amixer -q sset Master,0 toggle && pkill -RTMIN+10 i3blocks")

  bindsym [BrightnessUpSym] (ExecAction "xbacklight -inc 10")
  bindsym [BrightnessDownSym] (ExecAction "xbacklight -dec 10")

  Super Return ==> ExecAction "i3-sensible-terminal"
  Super W ==> Kill
  Super Slash ==> ExecAction "rofi -show drun"

  forWindow chrome [MoveToWorkspace W1]
  forWindow rubymine [MoveToWorkspace W2]
  forWindow slack [MoveToWorkspace W4]
  forWindow telegram [MoveToScratchpad, EnableSticky]

  bindsym [Mod4Sym, SpaceSym] FocusModeToggle
  bindsym [Mod4Sym, ShiftSym, SpaceSym] FloatingToggle

  Super Minus ==> ToggleScratchpad
  SuperShift Minus ==> [EnableSticky, MoveToScratchpad]

--   Super J ==> action' chrome focus
--   Super K ==> action' rubymine focus
--   Super Semicolon ==> action' slack focus
  Super T ==> action' terminal ToggleScratchpad
  Super O ==> ExecAction "emacsclient -c -n ~/notes/notes.org"
  bindsym [Mod4Sym, EqualSym] (action' telegram ToggleScratchpad)

  Super LeftBracket ==> FocusLeft
  Super RightBracket ==> FocusRight
  Super F ==> ToggleFullscreen
  Super H ==> SplitToggle

  Super J ==> FocusWorkspace W1
  Super K ==> FocusWorkspace W2
  Super L ==> FocusWorkspace W3
  Super Semicolon ==> FocusWorkspace W4
  Super Quote ==> FocusWorkspace W9

  Super N5 ==> FocusWorkspace W5
  Super N6 ==> FocusWorkspace W6
  Super N7 ==> FocusWorkspace W7
  Super N8 ==> FocusWorkspace W8
  Super N0 ==> FocusWorkspace W0

  mode (Super I) "Keyboard Layout Mode" $ do
    E ==> [ExecAction (setXkb "us"), exit]
    R ==> [ExecAction (setXkb "ru"), exit]
    U ==> [ExecAction (setXkb "ua"), exit]

  mode (Super Tilde) "i3 Management Mode" $ do
    C ==> [ReloadWM, exit]
    R ==> [RestartWM, exit]
    W ==> [ExecAction "rofi -show window", exit]

    mode L "Layout Mode" $ do
      S ==> [LayoutStacking, exit]
      T ==> [LayoutTabbed, exit]
      V ==> [LayoutSplitHorizontally, exit]
      H ==> [LayoutSplitVertically, exit]

  mode (Super R) "Resize Mode" $ do
    W ==> ResizeAction Grow Width 10
    N ==> ResizeAction Shrink Width 10
    H ==> ResizeAction Grow Height 10
    L ==> ResizeAction Shrink Height 10

  mode (Super M) "Move Mode" $ do
    H ==> MoveLeft 50
    L ==> MoveRight 50
    J ==> MoveDown 50
    K ==> MoveUp 50
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

main :: IO ()
main = putStrLn $ interpret $ flatten [Mode (ModeName "default") config]
