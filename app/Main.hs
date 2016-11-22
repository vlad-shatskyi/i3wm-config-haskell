module Main where

import Control.Monad.Free
import Data.List (intercalate)

data ActionCriteria = Instance String
                     | Class String
                     | Title String

class I3Serializable a where
  serialize :: a -> String

instance I3Serializable ActionCriteria where
  serialize (Instance name) = "instance=\"" ++ name ++ "\""
  serialize (Class name) = "class=\"" ++ name ++ "\""
  serialize (Title name) = "title=\"" ++ name ++ "\""

data KeyName = LowerVolumeSym
             | RaiseVolumeSym
             | MuteSym
             | BrightnessUpSym
             | BrightnessDownSym
             | EscapeSym
             | Mod4Sym
             | ShiftSym
             | SpaceSym
             | EqualSym
             | MinusSym
             | LeftBracketSym

instance I3Serializable KeyName where
  serialize LowerVolumeSym = "XF86AudioLowerVolume"
  serialize RaiseVolumeSym = "XF86AudioRaiseVolume"
  serialize MuteSym = "XF86AudioMute"
  serialize BrightnessUpSym = "XF86MonBrightnessUp"
  serialize BrightnessDownSym = "XF86MonBrightnessDown"
  serialize EscapeSym = "Escape"
  serialize Mod4Sym = "Mod4"
  serialize ShiftSym = "Shift"
  serialize SpaceSym = "space"
  serialize EqualSym = "equal"
  serialize MinusSym = "minus"
  serialize LeftBracketSym = "bracketLeft"

data Key = Tilde
         | One
         | Two
         | Three
         | Four
         | Five
         | Six
         | Seven
         | Eight
         | Nine
         | Zero
         | Minus
         | Q
         | W
         | E
         | R
         | T
         | Y
         | U
         | I
         | O
         | P
         | LeftBracket
         | RightBracket
         | Return
         | A
         | S
         | D
         | F
         | G
         | H
         | J
         | K
         | L
         | Semicolon
         | Quote
         | Z
         | X
         | C
         | V
         | B
         | N
         | M
         | Comma
         | Period
         | Slash
         | Mod4
         | Shift

instance I3Serializable Key where
  serialize Tilde = "49"
  serialize Shift = "Shift"
  serialize One = "10"
  serialize Two = "11"
  serialize Three = "12"
  serialize Four = "13"
  serialize Five = "14"
  serialize Six = "15"
  serialize Seven = "16"
  serialize Eight = "17"
  serialize Nine = "18"
  serialize Zero = "19"
  serialize Minus = "20"
  serialize Q = "24"
  serialize W = "25"
  serialize E = "26"
  serialize R = "27"
  serialize T = "28"
  serialize Y = "29"
  serialize U = "30"
  serialize I = "31"
  serialize O = "32"
  serialize P = "33"
  serialize LeftBracket = "34"
  serialize RightBracket = "35"
  serialize Return = "36"
  serialize A = "38"
  serialize S = "39"
  serialize D = "40"
  serialize F = "41"
  serialize G = "42"
  serialize H = "43"
  serialize J = "44"
  serialize K = "45"
  serialize L = "46"
  serialize Semicolon = "47"
  serialize Quote = "48"
  serialize Z = "52"
  serialize X = "53"
  serialize C = "54"
  serialize V = "55"
  serialize B = "56"
  serialize N = "57"
  serialize M = "58"
  serialize Comma = "59"
  serialize Period = "60"
  serialize Slash = "61"
  serialize Mod4 = "Mod4"

data MoveSubject = Window | Container
data MoveLocation = Workspace WorkspaceNumber | Scratchpad
data WorkspaceNumber = W1 | W2 | W3 | W4 | W5 | W6 | W7 | W8 | W9 | W0
data Layout = Stacking | Tabbed | ToggleSplit
data FocusActionTarget = ModeToggleFocusActionTarget | BasedOnCriteriaFocusActionTarget
data FloatingActionTarget = ToggleFloatingActionTarget
data GrowOrShrink = Grow | Shrink
data WidthOrHeight = Width | Height

instance I3Serializable Layout where
  serialize Stacking = "stacking"
  serialize Tabbed = "tabbed"
  serialize ToggleSplit = "toggle split"

instance I3Serializable GrowOrShrink where
  serialize Grow = "grow"
  serialize Shrink = "shrink"

instance I3Serializable WidthOrHeight where
  serialize Width = "width"
  serialize Height = "height"

data Action = ExecAction String
            | MoveAction MoveSubject MoveLocation
            | WorkspaceAction WorkspaceNumber
            | FocusAction FocusActionTarget
            | MoveLeft
            | MoveRight
            | MoveUp
            | MoveDown
            | MoveCenter
            | FocusLeft
            | FocusRight
            | FocusUp
            | FocusDown
            | ResizeAction GrowOrShrink WidthOrHeight Int
            | ToggleFullscreen
            | Kill
            | ReloadAction
            | RestartAction
            | FloatingAction FloatingActionTarget
            | ShowScratchpad
            | LayoutAction Layout
            | ModeAction ModeName

instance I3Serializable Action where
  serialize (ExecAction x) = "exec \"" ++ x ++ "\""
  serialize (LayoutAction x) = "layout " ++ serialize x
  serialize (MoveAction subject location) = "move " ++ serialize subject ++ " " ++ serialize location
  serialize (WorkspaceAction workspaceNumber) = "workspace " ++ serialize workspaceNumber
  serialize (ModeAction modeName) = "mode " ++ serialize modeName
  serialize (FocusAction target) = "focus " ++ serialize target
  serialize (FloatingAction target) = "floating " ++ serialize target
  serialize ShowScratchpad = "scratchpad show"
  serialize FocusLeft = "focus left"
  serialize FocusRight = "focus right"
  serialize FocusUp = "focus up"
  serialize FocusDown = "focus down"
  serialize MoveLeft = "move left"
  serialize MoveRight = "move right"
  serialize MoveUp = "move up"
  serialize MoveDown = "move down"
  serialize MoveCenter = "move position center"
  serialize (ResizeAction growOrShrink widthOrHeight amount) = "resize " ++ serialize growOrShrink ++ " " ++ serialize widthOrHeight ++ " " ++ show amount ++ " px or " ++ show amount ++ " ppt"
  serialize ToggleFullscreen = "fullscreen toggle"
  serialize Kill = "kill"
  serialize ReloadAction = "reload"
  serialize RestartAction = "restart"

instance I3Serializable FocusActionTarget where
  serialize ModeToggleFocusActionTarget = "mode_toggle"
  serialize BasedOnCriteriaFocusActionTarget = ""

instance I3Serializable FloatingActionTarget where
  serialize ToggleFloatingActionTarget = "toggle"

instance I3Serializable MoveSubject where
  serialize Window = ""
  serialize Container = "container"

instance I3Serializable MoveLocation where
  serialize (Workspace workspaceNumber) = "to workspace " ++ serialize workspaceNumber
  serialize Scratchpad = "scratchpad"

instance I3Serializable WorkspaceNumber where
  serialize W1 = "1"
  serialize W2 = "2"
  serialize W3 = "3"
  serialize W4 = "4"
  serialize W5 = "5"
  serialize W6 = "6"
  serialize W7 = "7"
  serialize W8 = "8"
  serialize W9 = "9"
  serialize W0 = "0"

data ModeName = ModeName String

instance I3Serializable ModeName where
  serialize (ModeName name) = "\"" ++ name ++ "\""

data ActionList = ActionList [ActionsWithCriteria]

instance I3Serializable ActionList where
  serialize (ActionList xs) = intercalate "; " (map serialize xs)

data ActionsWithCriteria = ActionsWithCriteria [ActionCriteria] [Action]

instance I3Serializable ActionsWithCriteria where
  serialize (ActionsWithCriteria [] action) = intercalate ", " (map serialize action)
  serialize (ActionsWithCriteria criteria action) = "[" ++ (intercalate " " (map serialize criteria)) ++ "] " ++ (intercalate ", " (map serialize action))

data I3ConfigStatement = I3Action ActionList
        | ExecAlways String
        | Font [String] Int
        | BindSym [KeyName] ActionList
        | BindCode [Key] ActionList
        | Bar String
        | HideEdgeBorders
        | ForWindow ActionsWithCriteria
        | Mode ModeName [I3ConfigStatement]

instance I3Serializable I3ConfigStatement where
  serialize (I3Action exec) = serialize exec
  serialize (ExecAlways x) = "exec_always " ++ x
  serialize (Font names size) = "font " ++ (intercalate ":" names) ++ " " ++ show size
  serialize (BindSym keys exec) = "bindsym " ++ (intercalate "+" (map serialize keys)) ++ " " ++ serialize exec
  serialize (BindCode codes exec) = "bindcode " ++ (intercalate "+" (map serialize codes)) ++ " " ++ serialize exec
  serialize (Bar command) = "bar {\n    status_command " ++ command ++ "\n    position top\n}"
  serialize HideEdgeBorders = "hide_edge_borders both"
  serialize (ForWindow x) = "for_window " ++ serialize x
  serialize (Mode name statements) = "mode " ++ (serialize name) ++ " {\n" ++ interpret statements ++ "\n}\n"

data Op next = Op I3ConfigStatement next deriving (Functor)

type Config = Free Op


liftF' :: I3ConfigStatement -> Config ()
liftF' x = liftF $ Op x ()

liftF'' = (liftF' .)

class ActionListConvertible x where
  toActionList :: x -> ActionList

instance ActionListConvertible Action where
  toActionList x = toActionList [x]

instance ActionListConvertible [Action] where
  toActionList xs = ActionList [ActionsWithCriteria [] xs]

instance ActionListConvertible ActionList where
  toActionList = id

actionList' :: [ActionCriteria] -> [Action] -> ActionList
actionList' cs xs = ActionList [ActionsWithCriteria cs xs]

action' :: [ActionCriteria] -> Action -> ActionList
action' cs x = actionList' cs [x]

focus = FocusAction BasedOnCriteriaFocusActionTarget

exec x = liftF' (I3Action (toActionList (ExecAction x)))
exec_always = liftF' . ExecAlways
font = liftF'' . Font
bindsym k a= liftF' (BindSym k (toActionList a))
bindcode k a = liftF' (BindCode k (toActionList a))
bar = liftF' . Bar
hide_edge_borders = liftF' HideEdgeBorders
for_window criteria action = liftF' (ForWindow (ActionsWithCriteria criteria [action]))
mode keys name config = bindcode keys (ModeAction modeName) >> liftF' (Mode modeName modeStatements)
  where modeName = ModeName name
        modeStatements = toList ((bindsym [EscapeSym] exitMode) >> config)

chrome = [Instance "google-chrome-unstable"]
rubymine = [Class "jetbrains-rubymine"]
slack = [Instance "slack"]
telegram = [Title "Telegram"]
terminal = [Instance "urxvt"]

exitMode = ModeAction (ModeName "default")

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

    -- mode [L] "Layout Mode" $ do
    --   bindcode [S] [LayoutAction Stacking, exitMode]
    --   bindcode [T] [LayoutAction Tabbed, exitMode]
    --   bindcode [E] [LayoutAction ToggleSplit, exitMode]

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

interpret :: [I3ConfigStatement] -> String
interpret xs = intercalate "\n" (map serialize xs)

toList :: Config a -> [I3ConfigStatement]
toList = reverse . toList' []
  where toList' accumulator (Pure _) = accumulator
        toList' accumulator (Free (Op i3 next)) = toList' (i3:accumulator) next

main :: IO ()
main = putStrLn $ interpret config
