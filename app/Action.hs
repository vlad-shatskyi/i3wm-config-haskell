module Action where

import Serializable
import Key
import Data.List (intercalate)

data WorkspaceNumber = W1 | W2 | W3 | W4 | W5 | W6 | W7 | W8 | W9 | W0
data FloatingActionTarget = ToggleFloatingActionTarget
data GrowOrShrink = Grow | Shrink
data WidthOrHeight = Width | Height

data Action = ExecAction String
            | WorkspaceAction WorkspaceNumber
            | ResizeAction GrowOrShrink WidthOrHeight Int
            | ToggleFullscreen
            | Kill

            | ReloadWM
            | RestartWM
            | ExitWM

            | MoveToScratchpad
            | ToggleScratchpad

            | Nop

            | SplitVertical
            | SplitHorizontal
            | SplitToggle

            | LayoutDefault
            | LayoutTabbed
            | LayoutStacking
            | LayoutSplitVertically
            | LayoutSplitHorizontally
            | LayoutToggleSplit
            | LayoutToggleAll

            | FocusLeft
            | FocusRight
            | FocusDown
            | FocusUp

            | FocusParent
            | FocusChild
            | FocusFloating
            | FocusTiling
            | FocusModeToggle

            | MoveLeft Int
            | MoveRight Int
            | MoveUp Int
            | MoveDown Int
            | MoveToCenter
            | MoveToPosition Int Int
            | MoveToMousePosition
            | MoveToWorkspace WorkspaceNumber

            | FloatingAction FloatingActionTarget
            | ModeAction ModeName
            | EnableSticky

instance Serializable Action where
  serialize (ExecAction x) = "exec \"" ++ x ++ "\""
  serialize (WorkspaceAction workspaceNumber) = "workspace " ++ serialize workspaceNumber
  serialize (ModeAction modeName) = "mode " ++ serialize modeName
  serialize (FloatingAction target) = "floating " ++ serialize target
  serialize MoveToScratchpad = "move scratchpad"
  serialize ToggleScratchpad = "scratchpad show"
  serialize Nop = "nop"
  serialize SplitVertical = "split vertical"
  serialize SplitHorizontal = "split horizontal"
  serialize SplitToggle = "split toggle"
  serialize LayoutDefault = "layout default"
  serialize LayoutTabbed = "layout tabbed"
  serialize LayoutStacking = "layout stacking"
  serialize LayoutSplitVertically = "layout splitv"
  serialize LayoutSplitHorizontally = "layout splith"
  serialize LayoutToggleSplit = "layout toggle split"
  serialize LayoutToggleAll = "layout toggle all"
  serialize FocusLeft = "focus left"
  serialize FocusRight = "focus right"
  serialize FocusDown = "focus down"
  serialize FocusUp = "focus up"
  serialize FocusParent = "focus parent"
  serialize FocusChild = "focus child"
  serialize FocusFloating = "focus floating"
  serialize FocusTiling = "focus tiling"
  serialize FocusModeToggle = "focus mode_toggle"
  serialize (MoveLeft x) = "move left " ++ show x
  serialize (MoveRight x) = "move right " ++ show x
  serialize (MoveUp x) = "move up " ++ show x
  serialize (MoveDown x) = "move down " ++ show x
  serialize MoveToCenter = "move position center"
  serialize (MoveToPosition x y) = "move position " ++ show x ++ " " ++ show y
  serialize MoveToMousePosition = "move position mouse"
  serialize (MoveToWorkspace workspaceNumber) = "move workspace " ++ serialize workspaceNumber
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

instance Serializable GrowOrShrink where
  serialize Grow = "grow"
  serialize Shrink = "shrink"

instance Serializable WidthOrHeight where
  serialize Width = "width"
  serialize Height = "height"

instance Serializable FloatingActionTarget where
  serialize ToggleFloatingActionTarget = "toggle"

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

data ShouldRelease = DontRelease | Release

instance Serializable ShouldRelease where
  serialize DontRelease = ""
  serialize Release = "--release"

data I3ConfigStatement = I3Action ActionList
        | ExecAlways String
        | Font [String] Int
        | BindSym [KeyName] ActionList
        | BindCode ShouldRelease Shortcut ActionList
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
  serialize (BindCode shouldRelease shortcut exec) = "bindcode " ++ serialize shouldRelease ++ " " ++ serialize shortcut ++ " " ++ serialize exec
  serialize (Bar command) = "bar {\n    status_command " ++ command ++ "\n    position top\n}"
  serialize HideEdgeBorders = "hide_edge_borders both"
  serialize (ForWindow x) = "for_window " ++ serialize x
  serialize (Mode (ModeName "default") statements) = interpret statements
  serialize (Mode name statements) = "mode " ++ serialize name ++ " {\n" ++ interpret statements ++ "\n}\n"
  serialize (Raw string) = string

interpret :: [I3ConfigStatement] -> String
interpret xs = intercalate "\n" (map serialize xs)
