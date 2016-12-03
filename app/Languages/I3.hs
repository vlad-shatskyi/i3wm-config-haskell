{-# LANGUAGE QuasiQuotes #-}

module Languages.I3 where

import DataTypes.Key
import DataTypes.Other

import Data.List (intercalate)
import Control.Monad.Free
import Data.String.Interpolate
import Data.String.Interpolate.Util

data Binding = BindSym [KeyName] ActionsWithCriteria | BindCode ShouldRelease Shortcut ActionsWithCriteria

data LanguageF next
  = ExecStatement ActionsWithCriteria next
  | ExecAlways String next
  | Font [String] Int next
  | Bar String next
  | HideEdgeBorders next
  | ForWindow ActionsWithCriteria next
  | ModeDefinition ModeName (Free BindingF next) next
  | Raw String next -- TODO: remove.
  deriving Functor

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

data ActionsWithCriteria = ActionsWithCriteria [ActionCriteria] [Action]

data BindingF next = BindingF Binding next deriving (Functor)

data TopLevelF next = LL (LanguageF next) | RR (BindingF next) deriving Functor

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

instance Show ActionsWithCriteria where
  show = \case
    ActionsWithCriteria [] action -> intercalate ", " (map show action)
    ActionsWithCriteria criteria action -> "[" ++ unwords (map show criteria) ++ "] " ++ intercalate ", " (map show action)

interpretLanguageF :: LanguageF (IO a) -> IO a
interpretLanguageF = \case
    ExecStatement exec next -> print exec >> next
    ExecAlways x next -> putStrLn [i|exec_always #{x}|] >> next
    Font names size next -> putStrLn [i|font #{intercalate ":" names} #{size}|] >> next
    HideEdgeBorders next -> putStrLn "hide_edge_borders both" >> next
    ForWindow x next -> putStrLn [i|for_window #{x}|] >> next
    Raw string next -> putStrLn string >> next
    Bar command next -> putStrLn (unindent [i|
      bar {
        status_command #{command}
        position top
      }
    |]) >> next
    ModeDefinition name bindings next -> do
      putStrLn [i|mode "#{name}" {|]
      _ <- iterM interpretBindingF bindings
      putStrLn "}"
      next

interpretBindingF :: BindingF (IO a) -> IO a
interpretBindingF (BindingF binding next) = printBinding binding >> next
  where printBinding (BindSym keys exec) = putStrLn [i|bindsym #{intercalate "+" (map show keys)} #{exec}|]
        printBinding (BindCode shouldRelease shortcut exec) = putStrLn [i|bindcode #{shouldRelease} #{shortcut} #{exec}|]

interpretTopLevelF :: TopLevelF (IO a) -> IO a
interpretTopLevelF (LL x) = interpretLanguageF x
interpretTopLevelF (RR x) = interpretBindingF x
