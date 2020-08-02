import XMonad
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.SpawnOnce
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.SetWMName
import XMonad.Layout.ThreeColumns
import XMonad.Layout.TwoPane
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.Combo
import XMonad.Layout.ResizableTile
import XMonad.Layout.WindowNavigation
import XMonad.Actions.GridSelect
import System.Exit

main = do
     xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig {
          modMask = mod4Mask
          , terminal = "xfce4-terminal"
          , borderWidth = 2
          , focusedBorderColor = "#ee4000"
          , normalBorderColor = "#9acd32"
          , manageHook = floatManageHook <+> manageDocks <+> manageHook defaultConfig
          , layoutHook = avoidStruts $ myLayout
          , handleEventHook = handleEventHook defaultConfig <+> docksEventHook
          , startupHook = startup
        } `additionalKeys`
        [ ((mod4Mask .|. shiftMask, xK_f), spawn "firefox")
        , ((mod4Mask .|. shiftMask, xK_v), spawn "gvim")
        , ((mod4Mask .|. shiftMask, xK_e), spawn "emacs")
        , ((mod4Mask .|. shiftMask, xK_q), spawn "xfce4-appfinder -c")
        , ((mod4Mask .|. shiftMask .|. mod1Mask, xK_h), spawn "~/.xmonad/script/toggle-xfce4-panel.sh")
        , ((mod4Mask .|. shiftMask .|. mod1Mask, xK_s), spawn "systemctl suspend")
        , ((mod4Mask .|. shiftMask .|. mod1Mask, xK_BackSpace), io exitSuccess)
        , ((mod4Mask .|. shiftMask .|. mod1Mask, xK_Delete), spawn "xscreensaver-command -lock")
        , ((mod4Mask, xK_p), spawn "xfce4-appfinder")
        , ((mod4Mask, xK_g), goToSelected myGridSelectConfig)
        , ((mod4Mask .|. controlMask, xK_space), sendMessage ToggleLayout)
        , ((mod4Mask .|. controlMask .|. shiftMask, xK_Right), sendMessage $ Move R)
        , ((mod4Mask .|. controlMask .|. shiftMask, xK_Left ), sendMessage $ Move L)
        , ((mod4Mask .|. controlMask .|. shiftMask, xK_Up   ), sendMessage $ Move U)
        , ((mod4Mask .|. controlMask .|. shiftMask, xK_Down ), sendMessage $ Move D)
        ]

defaultMyLayout = layoutHook defaultConfig
twoPaneLayout = toggleLayouts Full (TwoPane (3/100) (1/2))
combineTwoLayout = combineTwo (TwoPane (3/100) (1/2))
                   (Mirror $ ResizableTall 1 (3/100) (1/2) [])
                   (Mirror $ ResizableTall 1 (3/100) (1/2) [])

myLayout = toggleLayouts Full (ThreeColMid 1 (3/100) (2/5))
--           ||| twoPaneLayout
           ||| toggleLayouts Full (windowNavigation combineTwoLayout)

myGridSelectConfig = defaultGSConfig { gs_cellheight = 150, gs_cellwidth = 450 }

floatManageHook = composeAll
  [
    className =? "Xfce4-appfinder" --> doCenterFloat
  , className =? "Xfce4-settings-manager" --> doFloat
  ]

startup :: X()
startup = do
        setWMName "LG3D"
        spawnOnce "xrdb -merge ~/.xmonad/.Xresources"
        spawnOnce "xrandr --output LVDS1 --auto; xrandr --output VGA1 --auto --right-of LVDS1"
        spawnOnce "xscreensaver -no-splash"
        spawn "xfce4-panel -q; xfce4-panel -d"
        spawnOnce "nm-applet"
        spawnOnce "blueberry-tray"
        spawnOnce "yong -d"

