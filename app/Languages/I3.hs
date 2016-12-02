{-# LANGUAGE QuasiQuotes #-}

module Languages.I3 where

import DataTypes.Key
import DataTypes.Other
import Hoistable

import Data.List (intercalate)
import Data.Function
import Data.String.Interpolate
import Data.String.Interpolate.Util

data Binding = BindSym [KeyName] ActionList | BindCode ShouldRelease Shortcut ActionList

data I3 = ExecStatement ActionList
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

data BindingF next = BindingF Binding next deriving (Functor)
data LanguageF next = LanguageF I3 next deriving (Functor)

instance Hoistable BindingF LanguageF where
  hoist (BindingF binding next) = LanguageF (BindingStatement binding) next

instance Show I3 where
  show = \case
    ExecStatement exec -> show exec
    ExecAlways x -> [i|exec_always #{x}|]
    Font names size -> [i|font #{intercalate ":" names} #{size}|]
    BindingStatement (BindSym keys exec) -> [i|bindsym #{intercalate "+" (map show keys)} #{exec}|]
    BindingStatement (BindCode shouldRelease shortcut exec) -> [i|bindcode #{shouldRelease} #{shortcut} #{exec}|]
    HideEdgeBorders -> "hide_edge_borders both"
    ForWindow x -> [i|for_window #{x}|]
    Raw string -> string
    Bar command -> unindent [i|
      bar {
        status_command #{command}
        position top
      }
    |]
    ModeDefinition name bindings -> unindent [i|
      mode "#{name}" {
        #{bindings & map BindingStatement & map show & map (replicate 8 ' ' ++ ) & intercalate "\n"}
      }
    |]

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

interpretLanguageF :: LanguageF (IO a) -> IO a
interpretLanguageF (LanguageF statement next) = print statement >> next