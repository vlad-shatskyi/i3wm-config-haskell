module Languages.I3 where

import Serializable
import DataTypes.Key
import DataTypes.Other
import Data.List (intercalate)
import Data.Function

data Binding = BindSym [KeyName] ActionList | BindCode ShouldRelease Shortcut ActionList

data Statement = ExecStatement ActionList
        | ExecAlways String
        | Font [String] Int
        | BindingStatement Binding
        | Bar String
        | HideEdgeBorders
        | ForWindow ActionsWithCriteria
        | ModeDefinition ModeName [Binding]
        | Raw String -- TODO: remove.

data Action = Exec String
            | FocusWorkspace WorkspaceNumber

            | Resize GrowOrShrink WidthOrHeight Int
            | ResizeTo Int Int

            | CloseWindow

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

            | FloatingEnable
            | FloatingDisable
            | FloatingToggle

            | FullscreenEnable
            | FullscreenDisable
            | FullscreenToggle

            | StickyEnable
            | StickyDisable
            | StickyToggle

            | ActivateMode ModeName

data ActionCriteria = Instance String
                     | Class String
                     | Title String
                     | IsFloating
                     | IsCurrent

data ActionList = ActionList [ActionsWithCriteria]

data ActionsWithCriteria = ActionsWithCriteria [ActionCriteria] [Action]

instance Serializable Statement where
  serialize = \case
    ExecStatement exec -> serialize exec
    ExecAlways x -> "exec_always " ++ x
    Font names size -> "font " ++ intercalate ":" names ++ " " ++ show size
    BindingStatement (BindSym keys exec) -> "bindsym " ++ intercalate "+" (map serialize keys) ++ " " ++ serialize exec
    BindingStatement (BindCode shouldRelease shortcut exec) -> "bindcode " ++ serialize shouldRelease ++ " " ++ serialize shortcut ++ " " ++ serialize exec
    Bar command -> "bar {\n    status_command " ++ command ++ "\n    position top\n}"
    HideEdgeBorders -> "hide_edge_borders both"
    ForWindow x -> "for_window " ++ serialize x
    ModeDefinition name bindings -> "mode " ++ serialize name ++ " {\n" ++ (bindings & map BindingStatement & map serialize & intercalate "\n") ++ "\n}\n"
    Raw string -> string

instance Serializable Action where
  serialize = \case
    Exec x -> "exec \"" ++ x ++ "\""
    FocusWorkspace workspaceNumber -> "workspace " ++ serialize workspaceNumber
    ActivateMode modeName -> "mode " ++ serialize modeName
    FloatingEnable -> "floating enable"
    FloatingDisable -> "floating disable"
    FloatingToggle -> "floating toggle"
    StickyEnable -> "sticky enable"
    StickyDisable -> "sticky disable"
    StickyToggle -> "sticky toggle"
    FullscreenEnable -> "fullscreen enable"
    FullscreenDisable -> "fullscreen disable"
    FullscreenToggle -> "fullscreen toggle"
    MoveToScratchpad -> "move scratchpad"
    ToggleScratchpad -> "scratchpad show"
    Nop -> "nop"
    SplitVertical -> "split vertical"
    SplitHorizontal -> "split horizontal"
    SplitToggle -> "split toggle"
    LayoutDefault -> "layout default"
    LayoutTabbed -> "layout tabbed"
    LayoutStacking -> "layout stacking"
    LayoutSplitVertically -> "layout splitv"
    LayoutSplitHorizontally -> "layout splith"
    LayoutToggleSplit -> "layout toggle split"
    LayoutToggleAll -> "layout toggle all"
    FocusLeft -> "focus left"
    FocusRight -> "focus right"
    FocusDown -> "foI3Actioncus down"
    FocusUp -> "focus up"
    FocusParent -> "focus parent"
    FocusChild -> "focus child"
    FocusFloating -> "focus floating"
    FocusTiling -> "focus tiling"
    FocusModeToggle -> "focus mode_toggle"
    MoveLeft x -> "move left " ++ show x
    MoveRight x -> "move right " ++ show x
    MoveUp x -> "move up " ++ show x
    MoveDown x -> "move down " ++ show x
    MoveToCenter -> "move position center"
    MoveToPosition x y -> "move position " ++ show x ++ " " ++ show y
    MoveToMousePosition -> "move position mouse"
    MoveToWorkspace workspaceNumber -> "move workspace " ++ serialize workspaceNumber
    Resize growOrShrink widthOrHeight amount -> "resize " ++ serialize growOrShrink ++ " " ++ serialize widthOrHeight ++ " " ++ show amount ++ " px or " ++ show amount ++ " ppt"
    ResizeTo w h -> "resize set " ++ show w ++ " " ++ show h
    CloseWindow -> "kill"
    ReloadWM -> "reload"
    RestartWM -> "restart"
    ExitWM -> "exit"

instance Serializable ActionCriteria where
  serialize = \case
    Instance name -> "instance=\"" ++ name ++ "\""
    Class name -> "class=\"" ++ name ++ "\""
    Title name -> "title=\"" ++ name ++ "\""
    IsFloating -> "floating"
    IsCurrent -> "con_id=__focused__"

instance Serializable ActionList where
  serialize = \case
    ActionList xs -> intercalate "; " (map serialize xs)

instance Serializable ActionsWithCriteria where
  serialize = \case
    ActionsWithCriteria [] action -> intercalate ", " (map serialize action)
    ActionsWithCriteria criteria action -> "[" ++ unwords (map serialize criteria) ++ "] " ++ intercalate ", " (map serialize action)