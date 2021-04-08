import XMonad
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.SpawnOnce
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.SetWMName
import XMonad.Layout.Grid
import XMonad.Layout.ThreeColumns
import XMonad.Layout.TwoPane
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.Combo
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.WindowNavigation
import XMonad.Layout.BinarySpacePartition
import XMonad.Actions.GridSelect
import XMonad.Actions.GroupNavigation
import XMonad.Actions.WindowGo
import XMonad.Actions.TagWindows
import XMonad.Prompt
import System.Exit

main = do
     xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig {
          modMask = mod4Mask
          , terminal = "xfce4-terminal"
          , workspaces = myWorkspaces
          , borderWidth = 2
          , focusedBorderColor = "#ee4000"
          , normalBorderColor = "#9acd32"
          , manageHook = floatManageHook <+> manageDocks <+> manageHook defaultConfig
          , layoutHook = avoidStruts $ defaultMyLayout
          , handleEventHook = handleEventHook defaultConfig <+> docksEventHook
          , logHook = historyHook
          , startupHook = startup
        } `additionalKeys`
        [ ((mod4Mask .|. shiftMask, xK_f), spawn "firefox")
        , ((mod4Mask .|. shiftMask, xK_v), spawn "gvim")
        , ((mod4Mask .|. shiftMask, xK_e), spawn "emacs")
        , ((mod4Mask .|. shiftMask, xK_q), spawn "xfce4-appfinder -c")
        , ((mod4Mask .|. shiftMask, xK_t), spawn "Thunar")
        , ((mod4Mask .|. shiftMask, xK_p), spawn "xfce4-screenshooter --region --mouse --save ~/Pictures/$(date '+%Y-%m-%d_%H-%M-%S').png; Thunar ~/Pictures")
        , ((mod3Mask, xK_Return), runOrRaiseNext "xfce4-terminal" (className =? "Xfce4-terminal"))
        , ((mod3Mask, xK_f), runOrRaiseNext "firefox" (className =? "Firefox"))
        , ((mod3Mask, xK_e), runOrRaiseNext "emacs" (className =? "Emacs"))
        , ((mod3Mask, xK_w), runOrRaiseNext "Electronic Wechat" (className =? "Electron"))
        , ((mod3Mask, xK_i), runOrRaiseNext "jetbrains-idea" (className =? "jetbrains-idea-ce"))
        , ((mod3Mask, xK_BackSpace), nextMatch History (return True))
        , ((mod4Mask .|. shiftMask .|. mod1Mask, xK_h), spawn "~/.xmonad/script/toggle-xfce4-panel.sh")
        , ((mod4Mask .|. shiftMask .|. mod1Mask, xK_s), spawn "systemctl suspend")
        , ((mod4Mask .|. shiftMask .|. mod1Mask, xK_BackSpace), io exitSuccess)
        , ((mod3Mask .|. shiftMask, xK_c), kill)
        , ((mod4Mask .|. shiftMask .|. mod1Mask, xK_Delete), spawn "xscreensaver-command -lock")
        , ((mod4Mask, xK_p), spawn "xfce4-appfinder")
        , ((mod4Mask, xK_g), goToSelected myGridSelectConfig)
        , ((mod4Mask .|. controlMask, xK_space), sendMessage ToggleLayout)
        , ((mod4Mask .|. mod1Mask, xK_r), sendMessage Rotate)
        , ((mod4Mask .|. mod1Mask, xK_s), sendMessage XMonad.Layout.BinarySpacePartition.Swap)
        , ((mod4Mask .|. mod1Mask, xK_p), sendMessage FocusParent)
        , ((mod4Mask .|. mod1Mask, xK_a), sendMessage Balance)
        , ((mod4Mask .|. shiftMask, xK_a), sendMessage Equalize)
        , ((mod4Mask .|. mod1Mask, xK_n), sendMessage SelectNode)
        , ((mod4Mask .|. mod1Mask, xK_m), sendMessage MoveNode)
        , ((mod4Mask .|. mod1Mask, xK_Left), sendMessage $ MoveSplit L)
        , ((mod4Mask .|. mod1Mask, xK_Right), sendMessage $ MoveSplit R)
        , ((mod4Mask .|. mod1Mask, xK_Up), sendMessage $ MoveSplit U)
        , ((mod4Mask .|. mod1Mask, xK_Down), sendMessage $ MoveSplit D)
        , ((mod4Mask .|. controlMask, xK_Left), sendMessage $ RotateL)
        , ((mod4Mask .|. controlMask, xK_Right), sendMessage $ RotateR)
--        , ((mod4Mask .|. controlMask, xK_Up), sendMessage $ FlipH)
--        , ((mod4Mask .|. controlMask, xK_Down), sendMessage $ FlipV)
        , ((mod4Mask .|. controlMask .|. shiftMask, xK_Right), sendMessage $ Move R)
        , ((mod4Mask .|. controlMask .|. shiftMask, xK_Left ), sendMessage $ Move L)
        , ((mod4Mask .|. controlMask .|. shiftMask, xK_Up   ), sendMessage $ Move U)
        , ((mod4Mask .|. controlMask .|. shiftMask, xK_Down ), sendMessage $ Move D)
        , ((mod4Mask .|. controlMask .|. shiftMask, xK_Left), sendMessage $ Move L)
        , ((mod4Mask .|. controlMask .|. shiftMask, xK_Up), sendMessage $ Move U)
        , ((mod4Mask .|. controlMask .|. shiftMask, xK_Down), sendMessage $ Move D)
        , ((mod4Mask, xK_Right), (sendMessage $ Go R))
        , ((mod4Mask, xK_Left ), (sendMessage $ Go L))
        , ((mod4Mask, xK_Up   ), (sendMessage $ Go U))
        , ((mod4Mask, xK_Down ), (sendMessage $ Go D))
-- https://hackage.haskell.org/package/xmonad-contrib-0.16/docs/XMonad-Actions-TagWindows.html
        , ((mod3Mask .|. mod1Mask,    xK_a), withFocused (addTag "a"))
        , ((mod3Mask .|. shiftMask,   xK_a), withFocused (delTag "a"))
        , ((mod3Mask .|. controlMask, xK_a), focusUpTaggedGlobal "a")
        , ((mod3Mask .|. mod1Mask,    xK_b), withFocused (addTag "b"))
        , ((mod3Mask .|. shiftMask,   xK_b), withFocused (delTag "b"))
        , ((mod3Mask .|. controlMask, xK_b), focusUpTaggedGlobal "b")
        , ((mod3Mask .|. mod1Mask,    xK_c), withFocused (addTag "c"))
        , ((mod3Mask .|. shiftMask,   xK_c), withFocused (delTag "c"))
        , ((mod3Mask .|. controlMask, xK_c), focusUpTaggedGlobal "c")
        , ((mod3Mask .|. mod1Mask,    xK_d), withFocused (addTag "d"))
        , ((mod3Mask .|. shiftMask,   xK_d), withFocused (delTag "d"))
        , ((mod3Mask .|. controlMask, xK_d), focusUpTaggedGlobal "d")
        , ((mod3Mask .|. mod1Mask,    xK_e), withFocused (addTag "e"))
        , ((mod3Mask .|. shiftMask,   xK_e), withFocused (delTag "e"))
        , ((mod3Mask .|. controlMask, xK_e), focusUpTaggedGlobal "e")
        , ((mod3Mask .|. mod1Mask,    xK_f), withFocused (addTag "f"))
        , ((mod3Mask .|. shiftMask,   xK_f), withFocused (delTag "f"))
        , ((mod3Mask .|. controlMask, xK_f), focusUpTaggedGlobal "f")
        , ((mod3Mask .|. mod1Mask,    xK_g), withFocused (addTag "g"))
        , ((mod3Mask .|. shiftMask,   xK_g), withFocused (delTag "g"))
        , ((mod3Mask .|. controlMask, xK_g), focusUpTaggedGlobal "g")
        , ((mod3Mask .|. mod1Mask,    xK_h), withFocused (addTag "h"))
        , ((mod3Mask .|. shiftMask,   xK_h), withFocused (delTag "h"))
        , ((mod3Mask .|. controlMask, xK_h), focusUpTaggedGlobal "h")
        , ((mod3Mask .|. mod1Mask,    xK_i), withFocused (addTag "i"))
        , ((mod3Mask .|. shiftMask,   xK_i), withFocused (delTag "i"))
        , ((mod3Mask .|. controlMask, xK_i), focusUpTaggedGlobal "i")
        , ((mod3Mask .|. mod1Mask,    xK_j), withFocused (addTag "j"))
        , ((mod3Mask .|. shiftMask,   xK_j), withFocused (delTag "j"))
        , ((mod3Mask .|. controlMask, xK_j), focusUpTaggedGlobal "j")
        , ((mod3Mask .|. mod1Mask,    xK_k), withFocused (addTag "k"))
        , ((mod3Mask .|. shiftMask,   xK_k), withFocused (delTag "k"))
        , ((mod3Mask .|. controlMask, xK_k), focusUpTaggedGlobal "k")
        , ((mod3Mask .|. mod1Mask,    xK_l), withFocused (addTag "l"))
        , ((mod3Mask .|. shiftMask,   xK_l), withFocused (delTag "l"))
        , ((mod3Mask .|. controlMask, xK_l), focusUpTaggedGlobal "l")
        , ((mod3Mask .|. mod1Mask,    xK_m), withFocused (addTag "m"))
        , ((mod3Mask .|. shiftMask,   xK_m), withFocused (delTag "m"))
        , ((mod3Mask .|. controlMask, xK_m), focusUpTaggedGlobal "m")
        , ((mod3Mask .|. mod1Mask,    xK_n), withFocused (addTag "n"))
        , ((mod3Mask .|. shiftMask,   xK_n), withFocused (delTag "n"))
        , ((mod3Mask .|. controlMask, xK_n), focusUpTaggedGlobal "n")
        , ((mod3Mask .|. mod1Mask,    xK_o), withFocused (addTag "o"))
        , ((mod3Mask .|. shiftMask,   xK_o), withFocused (delTag "o"))
        , ((mod3Mask .|. controlMask, xK_o), focusUpTaggedGlobal "o")
        , ((mod3Mask .|. mod1Mask,    xK_p), withFocused (addTag "p"))
        , ((mod3Mask .|. shiftMask,   xK_p), withFocused (delTag "p"))
        , ((mod3Mask .|. controlMask, xK_p), focusUpTaggedGlobal "p")
        , ((mod3Mask .|. mod1Mask,    xK_q), withFocused (addTag "q"))
        , ((mod3Mask .|. shiftMask,   xK_q), withFocused (delTag "q"))
        , ((mod3Mask .|. controlMask, xK_q), focusUpTaggedGlobal "q")
        , ((mod3Mask .|. mod1Mask,    xK_r), withFocused (addTag "r"))
        , ((mod3Mask .|. shiftMask,   xK_r), withFocused (delTag "r"))
        , ((mod3Mask .|. controlMask, xK_r), focusUpTaggedGlobal "r")
        , ((mod3Mask .|. mod1Mask,    xK_s), withFocused (addTag "s"))
        , ((mod3Mask .|. shiftMask,   xK_s), withFocused (delTag "s"))
        , ((mod3Mask .|. controlMask, xK_s), focusUpTaggedGlobal "s")
        , ((mod3Mask .|. mod1Mask,    xK_t), withFocused (addTag "t"))
        , ((mod3Mask .|. shiftMask,   xK_t), withFocused (delTag "t"))
        , ((mod3Mask .|. controlMask, xK_t), focusUpTaggedGlobal "t")
        , ((mod3Mask .|. mod1Mask,    xK_u), withFocused (addTag "u"))
        , ((mod3Mask .|. shiftMask,   xK_u), withFocused (delTag "u"))
        , ((mod3Mask .|. controlMask, xK_u), focusUpTaggedGlobal "u")
        , ((mod3Mask .|. mod1Mask,    xK_v), withFocused (addTag "v"))
        , ((mod3Mask .|. shiftMask,   xK_v), withFocused (delTag "v"))
        , ((mod3Mask .|. controlMask, xK_v), focusUpTaggedGlobal "v")
        , ((mod3Mask .|. mod1Mask,    xK_w), withFocused (addTag "w"))
        , ((mod3Mask .|. shiftMask,   xK_w), withFocused (delTag "w"))
        , ((mod3Mask .|. controlMask, xK_w), focusUpTaggedGlobal "w")
        , ((mod3Mask .|. mod1Mask,    xK_x), withFocused (addTag "x"))
        , ((mod3Mask .|. shiftMask,   xK_x), withFocused (delTag "x"))
        , ((mod3Mask .|. controlMask, xK_x), focusUpTaggedGlobal "x")
        , ((mod3Mask .|. mod1Mask,    xK_y), withFocused (addTag "y"))
        , ((mod3Mask .|. shiftMask,   xK_y), withFocused (delTag "y"))
        , ((mod3Mask .|. controlMask, xK_y), focusUpTaggedGlobal "y")
        , ((mod3Mask .|. mod1Mask,    xK_z), withFocused (addTag "z"))
        , ((mod3Mask .|. shiftMask,   xK_z), withFocused (delTag "z"))
        , ((mod3Mask .|. controlMask, xK_z), focusUpTaggedGlobal "z")
        , ((mod4Mask, xK_0), (windows $ W.greedyView $ myWorkspaces !! 9))
        , ((mod4Mask, xK_F1), (windows $ W.greedyView $ myWorkspaces !! 10))
        , ((mod4Mask, xK_F2), (windows $ W.greedyView $ myWorkspaces !! 11))
        , ((mod4Mask, xK_F3), (windows $ W.greedyView $ myWorkspaces !! 12))
        , ((mod4Mask, xK_F4), (windows $ W.greedyView $ myWorkspaces !! 13))
        , ((mod4Mask, xK_F5), (windows $ W.greedyView $ myWorkspaces !! 14))
        , ((mod4Mask, xK_F6), (windows $ W.greedyView $ myWorkspaces !! 15))
        , ((mod4Mask, xK_F7), (windows $ W.greedyView $ myWorkspaces !! 16))
        , ((mod4Mask, xK_F8), (windows $ W.greedyView $ myWorkspaces !! 17))
        , ((mod4Mask, xK_F9), (windows $ W.greedyView $ myWorkspaces !! 18))
        , ((mod4Mask, xK_F10), (windows $ W.greedyView $ myWorkspaces !! 19))
        , ((mod4Mask .|. shiftMask, xK_0), (windows $ W.shift $ myWorkspaces !! 9))
        , ((mod4Mask .|. shiftMask, xK_F1), (windows $ W.shift $ myWorkspaces !! 10))
        , ((mod4Mask .|. shiftMask, xK_F2), (windows $ W.shift $ myWorkspaces !! 11))
        , ((mod4Mask .|. shiftMask, xK_F3), (windows $ W.shift $ myWorkspaces !! 12))
        , ((mod4Mask .|. shiftMask, xK_F4), (windows $ W.shift $ myWorkspaces !! 13))
        , ((mod4Mask .|. shiftMask, xK_F5), (windows $ W.shift $ myWorkspaces !! 14))
        , ((mod4Mask .|. shiftMask, xK_F6), (windows $ W.shift $ myWorkspaces !! 15))
        , ((mod4Mask .|. shiftMask, xK_F7), (windows $ W.shift $ myWorkspaces !! 16))
        , ((mod4Mask .|. shiftMask, xK_F8), (windows $ W.shift $ myWorkspaces !! 17))
        , ((mod4Mask .|. shiftMask, xK_F9), (windows $ W.shift $ myWorkspaces !! 18))
        , ((mod4Mask .|. shiftMask, xK_F10), (windows $ W.shift $ myWorkspaces !! 19))
        ]

defaultMyLayout = toggleLayouts (noBorders Full) usedLayout
usedLayout = windowNavigation (
  emptyBSP
  ||| Tall 1 (3/100) (1/2)
  ||| Grid
  )

twoPaneLayout = toggleLayouts (noBorders Full) (TwoPane (3/100) (1/2))

combineTwoLayout = combineTwo (TwoPane (3/100) (1/2))
                   (Mirror $ ResizableTall 1 (3/100) (1/2) [])
                   (Mirror $ ResizableTall 1 (3/100) (1/2) [])

myLayout = toggleLayouts (noBorders Full) (ThreeColMid 1 (3/100) (2/5))
--           ||| twoPaneLayout
           ||| toggleLayouts (noBorders Full) (windowNavigation combineTwoLayout)

myGridSelectConfig = defaultGSConfig { gs_cellheight = 150, gs_cellwidth = 450 }

floatManageHook = composeAll
  [
    className =? "Xfce4-appfinder" --> doCenterFloat
  , className =? "Xfce4-settings-manager" --> doFloat
  ]

myWorkspaces = ["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20"]

startup :: X()
startup = do
        setWMName "LG3D"
        spawnOnce "xrdb -merge ~/.xmonad/.Xresources"
        spawnOnce "xrandr --output LVDS1 --auto; xrandr --output VGA1 --auto --right-of LVDS1"
        spawnOnce "xscreensaver -no-splash"
        spawn "xfce4-panel -q; xfce4-panel -d"
        spawnOnce "nm-applet"
        spawnOnce "xfce4-power-manager"
        spawnOnce "blueberry-tray"
        spawnOnce "yong -d"

