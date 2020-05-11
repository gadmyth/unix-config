import XMonad
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Run(spawnPipe)
import XMonad.Hooks.DynamicLog
import System.IO
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.SetWMName

main = do
     xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig {
          modMask = mod4Mask
          , terminal = "xfce4-terminal"
          , borderWidth = 2
          , focusedBorderColor = "#ee4000"
          , normalBorderColor = "#9acd32"
          , manageHook = floatManageHook <+> manageDocks <+> manageHook defaultConfig
          , layoutHook = avoidStruts $ layoutHook defaultConfig
          , handleEventHook = handleEventHook defaultConfig <+> docksEventHook
          , startupHook = startup
        } `additionalKeys`
        [ ((mod4Mask .|. shiftMask, xK_f), spawn "firefox")
        , ((mod4Mask .|. shiftMask, xK_v), spawn "gvim")
        , ((mod4Mask .|. shiftMask, xK_e), spawn "emacs")
        , ((controlMask .|. mod1Mask, xK_Delete), spawn "xscreensaver-command -lock")
        , ((mod4Mask, xK_p), spawn "xfce4-appfinder")
        ]

floatManageHook = composeAll
  [
    className =? "Xfce4-appfinder" --> doFloat
  , className =? "Xfce4-settings-manager" --> doFloat
  ]

startup :: X()
startup = do
        setWMName "LG3D"
        spawn "xrdb -merge ~/.xmonad/.Xresources"
        spawn "xrandr --output LVDS1 --auto; xrandr --output VGA1 --auto --right-of LVDS1"
        spawn "xscreensaver -no-splash"
        spawn "xfce4-panel -q; xfce4-panel -d"
        spawn "nm-applet"
        spawn "blueberry-tray"
        spawn "for p in `ps aux| grep yong | grep -v grep | awk '{print $2}'`; do kill -9 $p; done; yong -d"

