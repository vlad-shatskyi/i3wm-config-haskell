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
  = ExecStatement String next
  | ExecAlways String next
  | Font [String] Int next
  | Bar String next
  | HideEdgeBorders next
  | ForWindow ActionsWithCriteria next
  | ModeDefinition ModeIdentifier (Free BindingF next) next
  | Raw String next -- TODO: remove.
  deriving Functor

data ActionF next
  = Exec String next
  | FocusWorkspace WorkspaceNumber next

  | Resize GrowOrShrink WidthOrHeight Int next
  | ResizeTo Int Int next

  | CloseWindow next

  | ReloadWM next
  | RestartWM next
  | ExitWM next

  | MoveToScratchpad next
  | ToggleScratchpad next

  | Nop next

  | SplitVertical next
  | SplitHorizontal next
  | SplitToggle next

  | LayoutDefault next
  | LayoutTabbed next
  | LayoutStacking next
  | LayoutSplitVertically next
  | LayoutSplitHorizontally next
  | LayoutToggleSplit next
  | LayoutToggleAll next

  | FocusLeft next
  | FocusRight next
  | FocusDown next
  | FocusUp next

  | FocusParent next
  | FocusChild next
  | FocusFloating next
  | FocusTiling next
  | FocusModeToggle next

  | MoveLeft Int next
  | MoveRight Int next
  | MoveUp Int next
  | MoveDown Int next
  | MoveToCenter next
  | MoveToPosition Int Int next
  | MoveToMousePosition next
  | MoveToWorkspace WorkspaceNumber next

  | FloatingEnable next
  | FloatingDisable next
  | FloatingToggle next

  | FullscreenEnable next
  | FullscreenDisable next
  | FullscreenToggle next

  | StickyEnable next
  | StickyDisable next
  | StickyToggle next

  | ActivateMode ModeIdentifier next
  deriving Functor

data ActionCriteria = Instance String
                     | Class String
                     | Title String
                     | IsFloating
                     | IsCurrent

data ActionsWithCriteria = ActionsWithCriteria [ActionCriteria] (Free ActionF ())

data BindingF next = BindingF Binding next deriving (Functor)

data TopLevelF next = LL (LanguageF next) | RR (BindingF next) deriving Functor

instance Show ActionCriteria where
  show = \case
    Instance name -> "instance=\"" ++ name ++ "\""
    Class name -> "class=\"" ++ name ++ "\""
    Title name -> [i|title="#{name}"|]
    IsFloating -> "floating"
    IsCurrent -> "con_id=__focused__"

interpretLanguageF :: LanguageF (IO a) -> IO a
interpretLanguageF = \case
    ExecStatement exec next -> putStrLn [i|exec #{exec}|] >> next
    ExecAlways x next -> putStrLn [i|exec_always #{x}|] >> next
    Font names size next -> putStrLn [i|font #{intercalate ":" names} #{size}|] >> next
    HideEdgeBorders next -> putStrLn "hide_edge_borders both" >> next
    ForWindow as next -> do
      putStr "for_window "
      putActionsWithCriteria as
      putChar '\n'
      next
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
      putStrLn "}\n"
      next

interpretBindingF :: BindingF (IO a) -> IO a
interpretBindingF (BindingF binding next) = printBinding binding >> next
  where printBinding (BindSym keys as) = do
          putStr [i|bindsym #{intercalate "+" (map show keys)} |]
          putActionsWithCriteria as
          putChar '\n'

        printBinding (BindCode shouldRelease shortcut as) = do
          putStr [i|bindcode #{shouldRelease} #{shortcut} |]
          putActionsWithCriteria as
          putChar '\n'


putActionsWithCriteria (ActionsWithCriteria criteria actionsF) = do
  putStr $ showCriteria criteria
  iterM interpretActionF actionsF
  putStr "nop"
  where showCriteria = \case
            [] -> ""
            criteria -> "[" ++ unwords (map show criteria) ++ "] "

interpretTopLevelF :: TopLevelF (IO a) -> IO a
interpretTopLevelF (LL x) = interpretLanguageF x
interpretTopLevelF (RR x) = interpretBindingF x

interpretActionF :: ActionF (IO a) -> IO a
interpretActionF = \case
    Exec x next -> putStr [i|exec "#{x}", |] >> next
    FocusWorkspace workspaceNumber next -> putStr [i|workspace #{workspaceNumber}, |] >> next
    ActivateMode modeName next -> putStr [i|mode "#{modeName}", |] >> next
    FloatingEnable next -> putStr "floating enable, " >> next
    FloatingDisable next -> putStr "floating disable, " >> next
    FloatingToggle next -> putStr "floating toggle, " >> next
    StickyEnable next -> putStr "sticky enable, " >> next
    StickyDisable next -> putStr "sticky disable, " >> next
    StickyToggle next -> putStr "sticky toggle, " >> next
    FullscreenEnable next -> putStr "fullscreen enable, " >> next
    FullscreenDisable next -> putStr "fullscreen disable, " >> next
    FullscreenToggle next -> putStr "fullscreen toggle, " >> next
    MoveToScratchpad next -> putStr "move scratchpad, " >> next
    ToggleScratchpad next -> putStr "scratchpad show, " >> next
    Nop next -> putStr "nop, " >> next
    SplitVertical next -> putStr "split vertical, " >> next
    SplitHorizontal next -> putStr "split horizontal, " >> next
    SplitToggle next -> putStr "split toggle, " >> next
    LayoutDefault next -> putStr "layout default, " >> next
    LayoutTabbed next -> putStr "layout tabbed, " >> next
    LayoutStacking next -> putStr "layout stacking, " >> next
    LayoutSplitVertically next -> putStr "layout splitv, " >> next
    LayoutSplitHorizontally next -> putStr "layout splith, " >> next
    LayoutToggleSplit next -> putStr "layout toggle split, " >> next
    LayoutToggleAll next -> putStr "layout toggle all, " >> next
    FocusLeft next -> putStr "focus left, " >> next
    FocusRight next -> putStr "focus right, " >> next
    FocusDown next -> putStr "foI3Actioncus down, " >> next
    FocusUp next -> putStr "focus up, " >> next
    FocusParent next -> putStr "focus parent, " >> next
    FocusChild next -> putStr "focus child, " >> next
    FocusFloating next -> putStr "focus floating, " >> next
    FocusTiling next -> putStr "focus tiling, " >> next
    FocusModeToggle next -> putStr "focus mode_toggle, " >> next
    MoveLeft x next -> putStr [i|move left #{x}, |] >> next
    MoveRight x next -> putStr [i|move right #{x}, |] >> next
    MoveUp x next -> putStr [i|move up #{x}, |] >> next
    MoveDown x next -> putStr [i|move down #{x}, |] >> next
    MoveToCenter next -> putStr "move position center, " >> next
    MoveToPosition x y next -> putStr [i|move position #{x} #{y}, |] >> next
    MoveToMousePosition next -> putStr "move position mouse, " >> next
    MoveToWorkspace workspaceNumber next -> putStr [i|move workspace #{workspaceNumber}, |] >> next
    Resize growOrShrink widthOrHeight amount next -> putStr [i|resize #{growOrShrink} #{widthOrHeight} #{amount} px or #{amount} ppt, |] >> next
    ResizeTo w h next -> putStr [i|resize set #{w} #{h}, |] >> next
    CloseWindow next -> putStr "kill, " >> next
    ReloadWM next -> putStr "reload, " >> next
    RestartWM next -> putStr "restart, " >> next
    ExitWM next -> putStr "exit, " >> next
