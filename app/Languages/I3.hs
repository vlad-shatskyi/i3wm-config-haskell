{-# LANGUAGE QuasiQuotes #-}

module Languages.I3 where

import DataTypes.Key
import DataTypes.Other
import Data.List (intercalate)
import Data.Function
import Data.String.Interpolate

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

instance Show Statement where
  show = \case
    ExecStatement exec -> show exec
    ExecAlways x -> [i|exec_always #{x}|]
    Font names size -> [i|font #{intercalate ":" names} #{size}|]
    BindingStatement (BindSym keys exec) -> [i|bindsym #{intercalate "+" (map show keys)} #{exec}|]
    BindingStatement (BindCode shouldRelease shortcut exec) -> [i|bindcode #{shouldRelease} #{shortcut} #{exec}|]
    Bar command -> "bar {\n    status_command " ++ command ++ "\n    position top\n}"
    HideEdgeBorders -> "hide_edge_borders both"
    ForWindow x -> [i|for_window #{x}|]
    ModeDefinition name bindings -> "mode " ++ show name ++ " {\n" ++ (bindings & map BindingStatement & map show & intercalate "\n") ++ "\n}\n"
    Raw string -> string

instance Show Action where
  show = \case
    Exec x -> [i|exec "#{x}"|]
    FocusWorkspace workspaceNumber -> [i|workspace #{workspaceNumber}|]
    ActivateMode modeName -> [i|mode #{modeName}|]
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
    MoveLeft x -> [i|move left #{x}|]
    MoveRight x -> [i|move right #{x}|]
    MoveUp x -> [i|move up #{x}|]
    MoveDown x -> [i|move down #{x}|]
    MoveToCenter -> "move position center"
    MoveToPosition x y -> [i|move position #{x} #{y}|]
    MoveToMousePosition -> "move position mouse"
    MoveToWorkspace workspaceNumber -> [i|move workspace #{workspaceNumber}|]
    Resize growOrShrink widthOrHeight amount -> [i|resize #{growOrShrink} #{widthOrHeight} #{amount} px or #{amount} ppt|]
    ResizeTo w h -> [i|resize set #{w} #{h}|]
    CloseWindow -> "kill"
    ReloadWM -> "reload"
    RestartWM -> "restart"
    ExitWM -> "exit"

instance Show ActionCriteria where
  show = \case
    Instance name -> "instance=\"" ++ name ++ "\""
    Class name -> "class=\"" ++ name ++ "\""
    Title name -> [i|title="#{name}"|]
    IsFloating -> "floating"
    IsCurrent -> "con_id=__focused__"

instance Show ActionList where
  show = \case
    ActionList xs -> intercalate "; " (map show xs)

instance Show ActionsWithCriteria where
  show = \case
    ActionsWithCriteria [] action -> intercalate ", " (map show action)
    ActionsWithCriteria criteria action -> "[" ++ unwords (map show criteria) ++ "] " ++ intercalate ", " (map show action)