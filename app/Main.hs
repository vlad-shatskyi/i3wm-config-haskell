module Main where

import Control.Monad.Free
import Data.List (intercalate)

data ActionCriteria = Instance String
                     | Class String
                     | Title String

instance Show ActionCriteria where
  show (Instance name) = "instance=\"" ++ name ++ "\""
  show (Class name) = "class=\"" ++ name ++ "\""
  show (Title name) = "title=\"" ++ name ++ "\""

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

instance Show KeyName where
  show LowerVolumeSym = "XF86AudioLowerVolume"
  show RaiseVolumeSym = "XF86AudioRaiseVolume"
  show MuteSym = "XF86AudioMute"
  show BrightnessUpSym = "XF86MonBrightnessUp"
  show BrightnessDownSym = "XF86MonBrightnessDown"
  show EscapeSym = "Escape"
  show Mod4Sym = "Mod4"
  show ShiftSym = "Shift"
  show SpaceSym = "space"
  show EqualSym = "equal"
  show MinusSym = "minus"
  show LeftBracketSym = "bracketLeft"

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

instance Show Key where
  show Tilde = "49"
  show Shift = "Shift"
  show One = "10"
  show Two = "11"
  show Three = "12"
  show Four = "13"
  show Five = "14"
  show Six = "15"
  show Seven = "16"
  show Eight = "17"
  show Nine = "18"
  show Zero = "19"
  show Minus = "20"
  show Q = "24"
  show W = "25"
  show E = "26"
  show R = "27"
  show T = "28"
  show Y = "29"
  show U = "30"
  show I = "31"
  show O = "32"
  show P = "33"
  show LeftBracket = "34"
  show RightBracket = "35"
  show Return = "36"
  show A = "38"
  show S = "39"
  show D = "40"
  show F = "41"
  show G = "42"
  show H = "43"
  show J = "44"
  show K = "45"
  show L = "46"
  show Semicolon = "47"
  show Quote = "48"
  show Z = "52"
  show X = "53"
  show C = "54"
  show V = "55"
  show B = "56"
  show N = "57"
  show M = "58"
  show Comma = "59"
  show Period = "60"
  show Slash = "61"
  show Mod4 = "Mod4"

data MoveSubject = Window | Container
data MoveLocation = Workspace WorkspaceNumber | Scratchpad
data WorkspaceNumber = W1 | W2 | W3 | W4 | W5 | W6 | W7 | W8 | W9 | W0
data Layout = Stacking | Tabbed | ToggleSplit
data FocusActionTarget = ModeToggleFocusActionTarget | BasedOnCriteriaFocusActionTarget
data FloatingActionTarget = ToggleFloatingActionTarget

data Action = ExecAction String
            | MoveAction MoveSubject MoveLocation
            | WorkspaceAction WorkspaceNumber
            | FocusAction FocusActionTarget
            | FocusLeft
            | FocusRight
            | FocusUp
            | FocusDown
            | ToggleFullscreen
            | Kill
            | ReloadAction
            | RestartAction
            | FloatingAction FloatingActionTarget
            | ShowScratchpad
            | LayoutAction Layout
            | ModeAction ModeName

instance Show Action where
  show (ExecAction x) = "exec " ++ x
  show (MoveAction subject location) = "move " ++ show subject ++ " " ++ show location
  show (WorkspaceAction workspaceNumber) = "workspace " ++ show workspaceNumber
  show (ModeAction modeName) = "mode " ++ show modeName
  show (FocusAction target) = "focus " ++ show target
  show (FloatingAction target) = "floating " ++ show target
  show ShowScratchpad = "scratchpad show"
  show FocusLeft = "focus left"
  show FocusRight = "focus right"
  show FocusUp = "focus up"
  show FocusDown = "focus down"
  show ToggleFullscreen = "fullscreen toggle"
  show Kill = "kill"
  show ReloadAction = "reload"
  show RestartAction = "restart"

instance Show FocusActionTarget where
  show ModeToggleFocusActionTarget = "mode_toggle"
  show BasedOnCriteriaFocusActionTarget = ""

instance Show FloatingActionTarget where
  show ToggleFloatingActionTarget = "toggle"

instance Show MoveSubject where
  show Window = ""
  show Container = "container"

instance Show MoveLocation where
  show (Workspace workspaceNumber) = "to workspace " ++ show workspaceNumber
  show Scratchpad = "scratchpad"

instance Show WorkspaceNumber where
  show W1 = "1"
  show W2 = "2"
  show W3 = "3"
  show W4 = "4"
  show W5 = "5"
  show W6 = "6"
  show W7 = "7"
  show W8 = "8"
  show W9 = "9"
  show W0 = "0"

data ModeName = ModeName String

instance Show ModeName where
  show (ModeName name) = "\"" ++ name ++ "\""

data ActionList = ActionList [ActionsWithCriteria]

instance Show ActionList where
  show (ActionList xs) = intercalate "; " (map show xs)

data ActionsWithCriteria = ActionsWithCriteria [ActionCriteria] [Action]

instance Show ActionsWithCriteria where
  show (ActionsWithCriteria [] action) = intercalate ", " (map show action)
  show (ActionsWithCriteria criteria action) = "[" ++ (intercalate " " (map show criteria)) ++ "] " ++ (intercalate ", " (map show action))

data I3ConfigStatement = I3Action ActionList
        | ExecAlways String
        | Font [String] Int
        | BindSym [KeyName] ActionList
        | BindCode [Key] ActionList
        | Bar String
        | HideEdgeBorders
        | ForWindow ActionsWithCriteria
        | Mode ModeName [I3ConfigStatement]

instance Show I3ConfigStatement where
  show (I3Action exec) = show exec
  show (ExecAlways x) = "exec_always " ++ x
  show (Font names size) = "font " ++ (intercalate ":" names) ++ " " ++ show size
  show (BindSym keys exec) = "bindsym " ++ (intercalate "+" (map show keys)) ++ " " ++ show exec
  show (BindCode codes exec) = "bindcode " ++ (intercalate "+" (map show codes)) ++ " " ++ show exec
  show (Bar command) = "bar {\n    status_command " ++ command ++ "\n    position top\n}"
  show HideEdgeBorders = "hide_edge_borders both"
  show (ForWindow x) = "for_window " ++ show x
  show (Mode name statements) = "mode " ++ (show name) ++ " {\n" ++ interpret statements ++ "\n}\n"

data Op next = Op I3ConfigStatement next deriving (Functor)

type Config = Free Op


liftF' :: I3ConfigStatement -> Config ()
liftF' x = liftF $ Op x ()

liftF'' = (liftF' .)

actionList' :: [ActionCriteria] -> [Action] -> ActionList
actionList' cs xs = ActionList [ActionsWithCriteria cs xs]

actionList :: [Action] -> ActionList
actionList xs = actionList' [] xs

action' :: [ActionCriteria] -> Action -> ActionList
action' cs x = actionList' cs [x]

action :: Action -> ActionList
action x = actionList' [] [x]

focus = FocusAction BasedOnCriteriaFocusActionTarget

exec x = liftF' (I3Action (action (ExecAction x)))
exec_always = liftF' . ExecAlways
font = liftF'' . Font
bindsym = liftF'' . BindSym
bindcode = liftF'' . BindCode
bar = liftF' . Bar
hide_edge_borders = liftF' HideEdgeBorders
for_window criteria action = liftF' (ForWindow (ActionsWithCriteria criteria [action]))
mode name config = liftF' (Mode name modeStatements)
  where modeStatements = toList ((bindsym [EscapeSym] (action (ModeAction defaultMode))) >> config)

chrome = [Instance "google-chrome-unstable"]
rubymine = [Class "jetbrains-rubymine"]
slack = [Instance "slack"]
telegram = [Title "Telegram"]
terminal = [Instance "urxvt"]

defaultMode = ModeName "default"
keyboardLayoutMode = ModeName "Keyboard Layout"
i3ManagementMode = ModeName "i3 Management"
resizeMode = ModeName "Resize Mode"
moveMode = ModeName "Move Mode"
rubyMineMode = ModeName "RubyMine Mode"
favoritesMode = ModeName "Favorites Mode"

setXkb layout = "setxkbmap " ++ layout ++ " && xmodmap .xmodmap"

config :: [I3ConfigStatement]
config = toList $ do
  exec_always "xinput set-prop 12 278 1" -- Enable Tapping.
  exec_always "xinput set-prop 12 280 0" -- Disable Tapping Drag.
  exec_always "xinput set-prop 12 286 0.85" -- Increase Accel Speed.
  exec_always "xinput set-prop 12 288 1" -- Enable natural scroll.
  exec_always "xmodmap ~/.xmodmap"

  exec "google-chrome-unstable"
  exec "slack"
  exec "telegram-desktop"

  font ["pango", "monospace"] 8

  bar "i3blocks"
  hide_edge_borders

  bindsym [RaiseVolumeSym] (action (ExecAction "amixer -q sset Master 5%+ unmute"))
  bindsym [LowerVolumeSym] (action (ExecAction "amixer -q sset Master 5%- unmute"))
  bindsym [MuteSym] (action (ExecAction "amixer -q sset Master,0 toggle"))

  bindsym [BrightnessUpSym] (action (ExecAction "xbacklight -inc 10"))
  bindsym [BrightnessDownSym] (action (ExecAction "xbacklight -dec 10"))

  bindcode [Mod4, Return] (action (ExecAction "i3-sensible-terminal"))
  bindcode [Mod4, W] (action Kill)
  bindcode [Mod4, Slash] (action (ExecAction "rofi -show drun"))
  bindcode [Mod4, I] (action (ModeAction keyboardLayoutMode))
  bindcode [Mod4, Tilde] (action (ModeAction i3ManagementMode))

  for_window chrome (MoveAction Container (Workspace W1))
  for_window rubymine (MoveAction Container (Workspace W2))
  for_window slack (MoveAction Container (Workspace W4))
  for_window telegram (MoveAction Window Scratchpad)

  bindsym [Mod4Sym, SpaceSym] (action (FocusAction ModeToggleFocusActionTarget))
  bindsym [Mod4Sym, ShiftSym, SpaceSym] (action (FloatingAction ToggleFloatingActionTarget))

  bindcode [Mod4, Minus] (action ShowScratchpad)
  bindcode [Mod4, Shift, Minus] (action (MoveAction Window Scratchpad))

  bindcode [Mod4, J] (action' chrome focus)
  bindcode [Mod4, N] (action' terminal ShowScratchpad)
  bindcode [Mod4, K] (action' rubymine focus)
  bindcode [Mod4, Semicolon] (action' slack focus)
  bindsym [Mod4Sym, EqualSym] (action' telegram ShowScratchpad)

  bindcode [Mod4, LeftBracket] (action FocusLeft)
  bindcode [Mod4, RightBracket] (action FocusRight)
  bindcode [Mod4, F] (action ToggleFullscreen)

  bindcode [Mod4, One] (action (WorkspaceAction W1))
  bindcode [Mod4, Two] (action (WorkspaceAction W2))
  bindcode [Mod4, Three] (action (WorkspaceAction W3))
  bindcode [Mod4, Four] (action (WorkspaceAction W4))
  bindcode [Mod4, Five] (action (WorkspaceAction W5))
  bindcode [Mod4, Six] (action (WorkspaceAction W6))
  bindcode [Mod4, Seven] (action (WorkspaceAction W7))
  bindcode [Mod4, Eight] (action (WorkspaceAction W8))
  bindcode [Mod4, Nine] (action (WorkspaceAction W9))
  bindcode [Mod4, Zero] (action (WorkspaceAction W0))

  mode keyboardLayoutMode $ do
    bindcode [E] (actionList [ExecAction (setXkb "us"), ModeAction defaultMode])
    bindcode [R] (actionList [ExecAction (setXkb "ru"), ModeAction defaultMode])
    bindcode [U] (actionList [ExecAction (setXkb "ua"), ModeAction defaultMode])

  mode i3ManagementMode $ do
    bindcode [C] (actionList [ReloadAction, ModeAction defaultMode])
    bindcode [R] (actionList [RestartAction, ModeAction defaultMode])
    bindcode [W] (actionList [ExecAction "rofi -show window", ModeAction defaultMode])


interpret :: [I3ConfigStatement] -> String
interpret = (intercalate "\n") . (map show)

toList :: Config a -> [I3ConfigStatement]
toList = reverse . toList' []
  where toList' accumulator (Pure _) = accumulator
        toList' accumulator (Free (Op i3 next)) = toList' (i3:accumulator) next

main :: IO ()
main = putStrLn $ interpret config
