module Action where

import Serializable
import Key
import Data.List (intercalate)

data MoveSubject = Window | Container
data MoveLocation = Workspace WorkspaceNumber | Scratchpad
data WorkspaceNumber = W1 | W2 | W3 | W4 | W5 | W6 | W7 | W8 | W9 | W0
data Layout = Stacking | Tabbed | ToggleSplit
data FocusActionTarget = ModeToggleFocusActionTarget | BasedOnCriteriaFocusActionTarget
data FloatingActionTarget = ToggleFloatingActionTarget
data GrowOrShrink = Grow | Shrink
data WidthOrHeight = Width | Height

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

            | ReloadWM
            | RestartWM
            | ExitWM

            | ToggleScratchpad

            | FloatingAction FloatingActionTarget
            | LayoutAction Layout
            | ModeAction ModeName
            | EnableSticky

instance Serializable Action where
  serialize (ExecAction x) = "exec \"" ++ x ++ "\""
  serialize (LayoutAction x) = "layout " ++ serialize x
  serialize (MoveAction subject location) = "move " ++ serialize subject ++ " " ++ serialize location
  serialize (WorkspaceAction workspaceNumber) = "workspace " ++ serialize workspaceNumber
  serialize (ModeAction modeName) = "mode " ++ serialize modeName
  serialize (FocusAction target) = "focus " ++ serialize target
  serialize (FloatingAction target) = "floating " ++ serialize target
  serialize ToggleScratchpad = "scratchpad show"
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
  serialize ReloadWM = "reload"
  serialize RestartWM = "restart"
  serialize ExitWM = "exit"
  serialize EnableSticky = "sticky enable"

data ModeName = ModeName String

instance Serializable ModeName where
  serialize (ModeName name) = "\"" ++ name ++ "\""

instance Serializable Layout where
  serialize Stacking = "stacking"
  serialize Tabbed = "tabbed"
  serialize ToggleSplit = "toggle split"

instance Serializable GrowOrShrink where
  serialize Grow = "grow"
  serialize Shrink = "shrink"

instance Serializable WidthOrHeight where
  serialize Width = "width"
  serialize Height = "height"

instance Serializable FocusActionTarget where
  serialize ModeToggleFocusActionTarget = "mode_toggle"
  serialize BasedOnCriteriaFocusActionTarget = ""

instance Serializable FloatingActionTarget where
  serialize ToggleFloatingActionTarget = "toggle"

instance Serializable MoveSubject where
  serialize Window = ""
  serialize Container = "container"

instance Serializable MoveLocation where
  serialize (Workspace workspaceNumber) = "to workspace " ++ serialize workspaceNumber
  serialize Scratchpad = "scratchpad"

instance Serializable WorkspaceNumber where
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

data ActionCriteria = Instance String
                     | Class String
                     | Title String
                     | IsFloating

instance Serializable ActionCriteria where
  serialize (Instance name) = "instance=\"" ++ name ++ "\""
  serialize (Class name) = "class=\"" ++ name ++ "\""
  serialize (Title name) = "title=\"" ++ name ++ "\""
  serialize IsFloating = "floating"

data ActionList = ActionList [ActionsWithCriteria]

instance Serializable ActionList where
  serialize (ActionList xs) = intercalate "; " (map serialize xs)

data ActionsWithCriteria = ActionsWithCriteria [ActionCriteria] [Action]

instance Serializable ActionsWithCriteria where
  serialize (ActionsWithCriteria [] action) = intercalate ", " (map serialize action)
  serialize (ActionsWithCriteria criteria action) = "[" ++ unwords (map serialize criteria) ++ "] " ++ intercalate ", " (map serialize action)

data I3ConfigStatement = I3Action ActionList
        | ExecAlways String
        | Font [String] Int
        | BindSym [KeyName] ActionList
        | BindCode Shortcut ActionList
        | Bar String
        | HideEdgeBorders
        | ForWindow ActionsWithCriteria
        | Mode ModeName [I3ConfigStatement]
        | Raw String

instance Serializable I3ConfigStatement where
  serialize (I3Action exec) = serialize exec
  serialize (ExecAlways x) = "exec_always " ++ x
  serialize (Font names size) = "font " ++ intercalate ":" names ++ " " ++ show size
  serialize (BindSym keys exec) = "bindsym " ++ intercalate "+" (map serialize keys) ++ " " ++ serialize exec
  serialize (BindCode shortcut exec) = "bindcode " ++ serialize shortcut ++ " " ++ serialize exec
  serialize (Bar command) = "bar {\n    status_command " ++ command ++ "\n    position top\n}"
  serialize HideEdgeBorders = "hide_edge_borders both"
  serialize (ForWindow x) = "for_window " ++ serialize x
  serialize (Mode (ModeName "default") statements) = interpret statements
  serialize (Mode name statements) = "mode " ++ serialize name ++ " {\n" ++ interpret statements ++ "\n}\n"
  serialize (Raw string) = string

interpret :: [I3ConfigStatement] -> String
interpret xs = intercalate "\n" (map serialize xs)
