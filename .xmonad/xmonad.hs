import XMonad
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
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
import XMonad.Layout.Hidden
import XMonad.Layout.Minimize
import XMonad.Actions.GridSelect
import XMonad.Actions.GroupNavigation
import XMonad.Actions.WindowGo
import XMonad.Actions.TagWindows
import XMonad.Actions.Minimize
import XMonad.Prompt
import XMonad.Prompt.Shell
import System.Exit

main = do
     xmonad $ withUrgencyHook NoUrgencyHook $ def {
          modMask = mod4Mask
          , terminal = "xfce4-terminal"
          , workspaces = myWorkspaces
          , borderWidth = 2
          , focusedBorderColor = "#ee4000"
          , normalBorderColor = "#9acd32"
          , manageHook = floatManageHook <+> manageDocks <+> manageHook def
          , layoutHook = avoidStruts $ defaultMyLayout
          , handleEventHook = handleEventHook def <+> docksEventHook
          , logHook = historyHook
          , startupHook = startup
        } `additionalKeys`
       (
        [ ((mod4Mask .|. shiftMask, xK_f), spawn "firefox")
        , ((mod4Mask .|. shiftMask, xK_v), spawn "gvim")
        , ((mod4Mask .|. shiftMask, xK_e), spawn "emacs")
        , ((mod4Mask .|. shiftMask, xK_q), spawn "xfce4-appfinder -c")
        , ((mod4Mask .|. shiftMask, xK_t), spawn "Thunar")
        , ((mod4Mask .|. shiftMask, xK_p), spawn "image_file=~/Pictures/$(date '+%Y-%m-%d_%H-%M-%S').png; xfce4-screenshooter --region --mouse --save ${image_file}; [[ -f ${image_file} ]] && Thunar ~/Pictures")
        , ((mod4Mask, xK_Return), shellPrompt myPromptConfig)
        , ((mod3Mask, xK_Return), runOrRaiseNext "xfce4-terminal" (className =? "Xfce4-terminal"))
        , ((mod3Mask, xK_f), runOrRaiseNext "firefox" (className =? "Firefox"))
        , ((mod3Mask, xK_e), runOrRaiseNext "emacs" (className =? "Emacs"))
        , ((mod3Mask, xK_w), runOrRaiseNext "wechat" (stringProperty "WM_NAME" =? "Electronic WeChat"))
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
        , ((mod4Mask, xK_h), withFocused hideWindow)
        , ((mod4Mask .|. shiftMask, xK_h), popNewestHiddenWindow)
--        , ((mod4Mask, xK_h), withFocused minimizeWindow)
--        , ((mod4Mask .|. shiftMask, xK_h), withLastMinimized maximizeWindowAndFocus)
        ]
        ++
        [((mod4Mask .|. m, k), windows $ f i)
        | (i, k) <- zip myWorkspaces ([xK_1 .. xK_9] ++ [xK_0] ++ [xK_F1 .. xK_F10])
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
        ]
        ++
        -- https://hackage.haskell.org/package/xmonad-contrib-0.16/docs/XMonad-Actions-TagWindows.html
        [((mod3Mask .|. m, k), f tag)
        | (k, tag) <- zip [xK_a .. xK_z] (map (:[]) ['a' .. 'z'])
        , (f, m) <- [(withFocused . addTag, mod1Mask), (withFocused . delTag, shiftMask), (focusUpTaggedGlobal, controlMask)]
        ]
       )

defaultMyLayout = toggleLayouts (noBorders Full) usedLayout
usedLayout = minimize (
  hiddenWindows (
  windowNavigation (
  emptyBSP
  ||| ThreeColMid 1 (3/100) (1/3)
--  ||| Tall 1 (3/100) (1/2)
--  ||| Grid
  )
  )
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
  , appName =? "emacs" --> doCenterFloat
  ]

myWorkspaces = map show [1..20 :: Int]

myPromptConfig = def
  {
    promptBorderWidth = 0
  , position = Bottom
  , defaultText = ""
  , alwaysHighlight = True
  , historySize = 1024
  , height = 40
  }

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

