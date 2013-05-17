import XMonad
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Run(spawnPipe)
import XMonad.Hooks.DynamicLog
import System.IO
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook

main = do
     xmproc <- spawnPipe "xmobar"
     xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig {
	  modMask = mod4Mask
	  , manageHook = manageDocks <+> manageHook defaultConfig
	  , layoutHook = avoidStruts $ layoutHook defaultConfig
	  , logHook = loghook xmproc
	} `additionalKeys`
	[ ((mod4Mask .|. shiftMask, xK_g), spawn "google-chrome")
	, ((mod4Mask .|. shiftMask, xK_v), spawn "gvim")
	, ((mod4Mask .|. shiftMask, xK_e), spawn "emacs") ]

loghook h = dynamicLogWithPP $ xmobarPP {
	ppOutput = hPutStrLn h
	, ppTitle = xmobarColor "green" "" . shorten 50
	, ppHiddenNoWindows = xmobarColor "grey" ""
}
