module Main where

import Key
import Action
import DSL

chrome = [Instance "google-chrome-unstable"]
rubymine = [Class "jetbrains-rubymine"]
slack = [Instance "slack"]
telegram = [Title "Telegram"]
terminal = [Instance "urxvt"]

setXkb layout = "setxkbmap " ++ layout ++ " && xmodmap .xmodmap && pkill -RTMIN+11 i3blocks"

config :: [I3ConfigStatement]
config = toList $ do
  exec_always "xinput set-prop 12 281 1" -- Enable Tapping.
  exec_always "xinput set-prop 12 283 0" -- Disable Tapping Drag.
  exec_always "xinput set-prop 12 289 0.85" -- Increase Accel Speed.
  exec_always "xinput set-prop 12 291 1" -- Enable natural scroll.
  exec_always "xmodmap ~/.xmodmap"

  exec "google-chrome-unstable"
  exec "slack"
  exec "telegram-desktop"

  font ["pango", "monospace"] 8

  bar "i3blocks"
  hide_edge_borders

  bindsym [RaiseVolumeSym] (ExecAction "amixer -q sset Master 5%+ unmute && pkill -RTMIN+10 i3blocks")
  bindsym [LowerVolumeSym] (ExecAction "amixer -q sset Master 5%- unmute && pkill -RTMIN+10 i3blocks")
  bindsym [MuteSym] (ExecAction "amixer -q sset Master,0 toggle && pkill -RTMIN+10 i3blocks")

  bindsym [BrightnessUpSym] (ExecAction "xbacklight -inc 10")
  bindsym [BrightnessDownSym] (ExecAction "xbacklight -dec 10")

  bindcode [Mod4, Return] (ExecAction "i3-sensible-terminal")
  bindcode [Mod4, W] Kill
  bindcode [Mod4, Slash] (ExecAction "rofi -show drun")

  for_window chrome (MoveAction Container (Workspace W1))
  for_window rubymine (MoveAction Container (Workspace W2))
  for_window slack (MoveAction Container (Workspace W4))
  for_window telegram (MoveAction Window Scratchpad)

  bindsym [Mod4Sym, SpaceSym] (FocusAction ModeToggleFocusActionTarget)
  bindsym [Mod4Sym, ShiftSym, SpaceSym] (FloatingAction ToggleFloatingActionTarget)

  bindcode [Mod4, Minus] ShowScratchpad
  bindcode [Mod4, Shift, Minus] (MoveAction Window Scratchpad)

  bindcode [Mod4, J] (action' chrome focus)
  bindcode [Mod4, N] (action' terminal ShowScratchpad)
  bindcode [Mod4, K] (action' rubymine focus)
  bindcode [Mod4, Semicolon] (action' slack focus)
  bindsym [Mod4Sym, EqualSym] (action' telegram ShowScratchpad)

  bindcode [Mod4, LeftBracket] FocusLeft
  bindcode [Mod4, RightBracket] FocusRight
  bindcode [Mod4, F] ToggleFullscreen

  bindcode [Mod4, One] (WorkspaceAction W1)
  bindcode [Mod4, Two] (WorkspaceAction W2)
  bindcode [Mod4, Three] (WorkspaceAction W3)
  bindcode [Mod4, Four] (WorkspaceAction W4)
  bindcode [Mod4, Five] (WorkspaceAction W5)
  bindcode [Mod4, Six] (WorkspaceAction W6)
  bindcode [Mod4, Seven] (WorkspaceAction W7)
  bindcode [Mod4, Eight] (WorkspaceAction W8)
  bindcode [Mod4, Nine] (WorkspaceAction W9)
  bindcode [Mod4, Zero] (WorkspaceAction W0)

  mode [Mod4, I] "Keyboard Layout Mode" $ do
    bindcode [E] [ExecAction (setXkb "us"), exitMode]
    bindcode [R] [ExecAction (setXkb "ru"), exitMode]
    bindcode [U] [ExecAction (setXkb "ua"), exitMode]

  mode [Mod4, Tilde] "i3 Management Mode" $ do
    bindcode [C] [ReloadAction, exitMode]
    bindcode [R] [RestartAction, exitMode]
    bindcode [W] [ExecAction "rofi -show window", exitMode]

    mode [L] "Layout Mode" $ do
      bindcode [S] [LayoutAction Stacking, exitMode]
      bindcode [T] [LayoutAction Tabbed, exitMode]
      bindcode [E] [LayoutAction ToggleSplit, exitMode]

  mode [Mod4, R] "Resize Mode" $ do
    bindcode [W] (ResizeAction Grow Width 10)
    bindcode [N] (ResizeAction Shrink Width 10)
    bindcode [H] (ResizeAction Grow Height 10)
    bindcode [L] (ResizeAction Shrink Height 10)

  mode [Mod4, M] "Move Mode" $ do
    bindcode [H] MoveLeft
    bindcode [L] MoveRight
    bindcode [J] MoveDown
    bindcode [K] MoveUp
    bindcode [C] [MoveCenter, exitMode]

main :: IO ()
main = putStrLn $ interpret $ flatten [Mode (ModeName "default") config]
