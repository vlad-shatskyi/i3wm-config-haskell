module Main where

import Languages.I3
import DataTypes.Key
import DataTypes.Other
import DSL
import Control.Monad.Free
import Data.List (intercalate)

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
mail = [Instance "wmail"]

setLayout layout = "setxkbmap " ++ layout ++ " && pkill -RTMIN+11 i3blocks"

setMicrophoneLevel :: Integer -> String
setMicrophoneLevel percent = "amixer set Capture " ++ show percent ++ "% && pkill -RTMIN+12 i3blocks"

press shortcut = "xdotool key --clearmodifiers " ++ shortcut
pressSequence = intercalate " && " . map press

moveStep = 50
resizeStep = 50
screenWidth = 3840
screenHeight = 2160

dockedWindowWidth = quot screenWidth 3 * 2
dockedWindowHeight = 140

moveTo w = do
  lift (MoveToWorkspace w)
  focus w
  exit

resizeProportionally gs = do
  lift (Resize gs Width resizeStep)
  lift (Resize gs Height resizeStep)
  lift MoveToCenter

startupPrograms :: [String] -> Free TopLevelF ()
startupPrograms = mapM_ execNoStartupId


execAlwaysList :: [String] -> Free TopLevelF ()
execAlwaysList = mapM_ execAlways


focusWindow = lift $ FocusChild


config :: Free TopLevelF ()
config = do
  execAlwaysList [ "xinput set-prop 12 281 1" -- Enable Tapping.
                 , "xinput set-prop 12 283 0" -- Disable Tapping Drag.
                 , "xinput set-prop 12 289 0.85" -- Increase Accel Speed.
                 , "xinput set-prop 12 291 1" -- Enable natural scroll.
                 , "setxkbmap -option altwin:swap_alt_win"
                 , "setxkbmap -option ctrl:nocaps"
                 , "libinput-gestures-setup start"
                 ]

  startupPrograms [ "/usr/lib/gnome-settings-daemon/gnome-settings-daemon"
                  , "dunst"
                  , "clipmenud"
                  , "emacs --daemon"
                  , "wmail"
                  ]

  raw "floating_modifier Mod4"
  raw $ "floating_maximum_size " ++ show screenWidth ++ " x " ++ show screenHeight
  raw "focus_follows_mouse no"

  raw "client.focused #31363B #31363B #FFFFFF #000000 #31363B"
  raw "client.focused_inactive #31363B #31363B #777777 #000000 #31363B"
  raw "client.unfocused #31363B #31363B #777777 #000000 #31363B"
  raw "client.background #31363B"
  raw $ unlines $ [ "bar {"
      , "  colors {"
      , "    background #31363B"
      , "    separator #31363B"
      , "    focused_workspace #31363B #31363B #FFFFFF"
      , "    active_workspace #31363B #31363B #FFFFFF"
      , "    inactive_workspace #31363B #31363B #777777"
      , "    urgent_workspace #BD5151 #BD5151 #FFF0F0"
      , "  }"
      , "  status_command i3blocks"
      , "  position top"
      , "}"
      ]

  exec' "google-chrome-unstable"
  exec' "slack"
  exec' "telegram-desktop"

  font ["pango", "Hack"] 9

--   bar "i3blocks"
  hideEdgeBorders ()

  RaiseVolumeSym ==> lift (Exec "amixer -q sset Master 5%+ unmute && pkill -RTMIN+10 i3blocks")
  LowerVolumeSym ==> lift (Exec "amixer -q sset Master 5%- unmute && pkill -RTMIN+10 i3blocks")
  MuteSym ==> lift (Exec "amixer -q sset Master,0 toggle && pkill -RTMIN+10 i3blocks")

  BrightnessUpSym ==> lift (Exec "xbacklight -inc 10")
  BrightnessDownSym ==> lift (Exec "xbacklight -dec 10")

  Super Return ==> lift (Exec "konsole")
  Super W ==> lift CloseWindow
  Super Slash ==> lift (Exec "rofi -show drun")
  SuperShift Slash ==> lift (Exec "rofi -modi \"Files:~/.config/rofi/rofi-file-browser.sh\" -show Files")

  forWindow chrome (lift (MoveToWorkspace W1))
  forWindow rubymine (lift (MoveToWorkspace W2))
  forWindow idea (lift (MoveToWorkspace W2))
  forWindow slack (lift (MoveToWorkspace W4))
  forWindow telegram $ do
    lift MoveToScratchpad
    lift StickyEnable
  forWindow emacs (lift FloatingEnable)
  forWindow videoPlayer (lift FullscreenEnable)
  forWindow mail (lift (MoveToWorkspace W5))

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
  Super Equal ==> ActionsWithCriteria telegram (lift ToggleScratchpad)

  Super LeftBracket ==> lift FocusLeft
  Super RightBracket ==> lift FocusRight
  Super F ==> lift FullscreenToggle
  Super H ==> lift SplitToggle

  Super J ==> focus W1
  Super K ==> focus W2
  Super L ==> focus W3
  Super Semicolon ==> focus W4
  Super Quote ==> focus W9

  Super N1 ==> focus W1
  Super N2 ==> focus W2
  Super N3 ==> focus W3
  Super N4 ==> focus W4
  Super N5 ==> focus W5
  Super N6 ==> focus W6
  Super N7 ==> focus W7
  Super N8 ==> focus W8
  Super N9 ==> focus W9
  Super N0 ==> focus W0

  keyboardLayoutMode <- mode "Keyboard Layout Mode" $ do
    E ==>^ lift (Exec (setLayout "us"))
    R ==>^ lift (Exec (setLayout "ru"))
    U ==>^ lift (Exec (setLayout "ua"))
    Super I ==>^ lift (Exec (setLayout "us"))
    Super O ==>^ lift (Exec (setLayout "ru"))
    Super P ==>^ lift (Exec (setLayout "ua"))

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
    P ==>^ lift FocusParent

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

    LeftBracket  ==> lift (MoveLeft moveStep)
    RightBracket ==> lift (MoveRight moveStep)

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

  audioMode <- mode "Audio Mode" $ do
    M  ==>^ lift (Exec (setMicrophoneLevel 0))
    N0  ==>^ lift (Exec (setMicrophoneLevel 0))
    N1 ==>^ lift (Exec (setMicrophoneLevel 10))
    N2 ==>^ lift (Exec (setMicrophoneLevel 20))
    N3 ==>^ lift (Exec (setMicrophoneLevel 30))
    N4 ==>^ lift (Exec (setMicrophoneLevel 40))
    N5 ==>^ lift (Exec (setMicrophoneLevel 50))
    N6 ==>^ lift (Exec (setMicrophoneLevel 60))
    N7 ==>^ lift (Exec (setMicrophoneLevel 70))
    N8 ==>^ lift (Exec (setMicrophoneLevel 80))
    N9 ==>^ lift (Exec (setMicrophoneLevel 90))
  Super A ==> lift (ActivateMode audioMode)

  Super P ==> lift (Exec ("i3-msg mode $(" ++ focusedWindowClass ++ ")"))

  _ <- mode "jetbrains-idea-ce" $ do
    OnRelease N ==>^ lift (Exec (press "ctrl+shift+n"))
    OnRelease F ==>^ lift (Exec (press "ctrl+shift+f"))
    OnRelease R ==>^ lift (Exec (pressSequence ["Escape", "alt+1", "shift+F6"]))
    OnRelease P ==>^ lift (Exec (pressSequence ["alt+f", "alt+r", "Down"]))

  _ <- mode "Google-chrome-unstable" $ do
    OnRelease H ==>^ lift (Exec (pressSequence ["ctrl+t", "ctrl+l", "h", "c", "k", "Return"]))

  selectionMode <- mode "Selection Mode" $ do
    OnRelease G ==>^ do
      ActionsWithCriteria chrome $ do
        lift (Exec (press "ctrl+c"))
        lift Focus
        lift (Exec (pressSequence ["ctrl+t", "ctrl+v", "Return"]))

  Super S ==> lift (ActivateMode selectionMode)

main :: IO ()
main = iterM Languages.I3.interpretTopLevelF config
