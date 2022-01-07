import Data.Monoid (appEndo)
import XMonad hiding ( (|||) )
import XMonad.Core
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Util.SpawnOnce
import XMonad.Util.Themes
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
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Renamed
import XMonad.Layout.SubLayouts
import XMonad.Layout.Tabbed
import XMonad.Layout.SimpleDecoration
import XMonad.Layout.Simplest
import XMonad.Actions.CycleWS
import XMonad.Actions.CopyWindow
import XMonad.Actions.GridSelect
import XMonad.Actions.GroupNavigation
import XMonad.Actions.WindowGo
import XMonad.Actions.TagWindows
import XMonad.Actions.Minimize
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.ConfirmPrompt
import XMonad.Prompt.XMonad
import Graphics.X11.ExtraTypes.XF86
import System.Posix.Process
import System.IO
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
        -- applications
        [ ((mod4Mask .|. shiftMask, xK_f), spawn "firefox")
        , ((mod4Mask .|. shiftMask, xK_g), spawn "google-chrome")
        , ((mod4Mask .|. shiftMask, xK_v), spawn "gvim")
        , ((mod4Mask .|. shiftMask, xK_e), spawn "emacs")
        , ((mod4Mask .|. shiftMask, xK_s), spawn "emacsclient -c")
        , ((mod4Mask .|. shiftMask, xK_q), spawn "xfce4-appfinder -c")
        , ((mod4Mask .|. shiftMask, xK_d), spawn "Thunar")
        , ((mod4Mask .|. shiftMask, xK_p), spawn "image_file=~/Pictures/$(date '+%Y-%m-%d_%H-%M-%S').png; xfce4-screenshooter --region --mouse --save ${image_file}; [[ -f ${image_file} ]] && echo -n ${image_file} | xclip -selection c")
        , ((mod4Mask .|. shiftMask .|. controlMask, xK_p), spawn "image_file=~/Pictures/$(date '+%Y-%m-%d_%H-%M-%S').png; xfce4-screenshooter --region --mouse --save ${image_file}; [[ -f ${image_file} ]] && Thunar ~/Pictures")
        , ((mod4Mask, xK_r), shellPrompt myPromptConfig)
        , ((mod4Mask, xK_x), xmonadPromptC myXmonadCmds myPromptConfig)
        , ((mod4Mask .|. shiftMask, xK_r), prompt ("xfce4-terminal" ++ " -H -x") myPromptConfig)
        , ((mod5Mask, xK_Return), runOrRaiseNext "xfce4-terminal" (className =? "Xfce4-terminal"))
        , ((mod5Mask, xK_f), runOrRaiseNext "firefox" (className =? "Firefox"))
        , ((mod5Mask, xK_g), runOrRaiseNext "google-chrome" (className =? "Google-chrome"))
        , ((mod5Mask, xK_e), runOrRaiseNext "emacs" (className =? "Emacs"))
        , ((mod5Mask, xK_w), runOrRaiseNext "wechat" (className =? "Electron"))
        , ((mod5Mask, xK_i), runOrRaiseNext "jetbrains-idea" (className =? "jetbrains-idea-ce"))
        , ((mod5Mask, xK_t), runOrRaiseNext "xclock" (className =? "XClock"))
        -- system tools
        , ((mod4Mask, xK_BackSpace), nextMatch History (return True))
        -- toggle workspace, xK_grave is "`", defined in /usr/include/X11/keysymdef.h, detected by `xev` Linux command
        , ((mod4Mask, xK_grave), toggleWSWithHint)
        , ((mod4Mask, xK_i), notifyCurrentWSHint)
        , ((mod4Mask .|. shiftMask .|. mod1Mask, xK_h), spawn "~/.xmonad/script/toggle-xfce4-panel.sh")
        , ((mod4Mask .|. shiftMask .|. mod1Mask, xK_s), confirmPrompt myPromptConfig "Suspend?" $ spawn "systemctl suspend")
        , ((mod4Mask .|. shiftMask .|. mod1Mask, xK_Delete), confirmPrompt myPromptConfig "Lock Screen?" $ spawn "xscreensaver-command -lock")
        -- git clone https://github.com/jarun/xtrlock /opt/xtrlock; cd /opt/xtrlock; sudo make; sudo make install
        , ((mod4Mask .|. shiftMask .|. mod1Mask, xK_l), confirmPrompt myPromptConfig "Lock Screen?" $ spawn "xtrlock")
        , ((mod5Mask .|. shiftMask, xK_c), kill)
        , ((mod4Mask .|. shiftMask .|. mod1Mask, xK_c), kill1)
        , ((mod4Mask, xK_v), spawn "sleep 0.1; xdotool type --delay 0 \"$(xsel)\"")
        , ((mod4Mask, xK_p), spawn "xfce4-appfinder")
        , ((mod4Mask, xK_g), goToSelected myGridSelectConfig)
        -- audio
        , ((0, xF86XK_AudioRaiseVolume), spawn "pactl set-sink-volume $(pactl get-default-sink) +5%")
        , ((shiftMask, xF86XK_AudioRaiseVolume), spawn "pactl set-sink-volume $(pactl get-default-sink) +20%")
        , ((0, xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume $(pactl get-default-sink) -5%")
        , ((shiftMask , xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume $(pactl get-default-sink) -20%")
        , ((0, xF86XK_AudioMute), spawn "pactl set-sink-mute $(pactl get-default-sink) toggle")
        -- layouts
        , ((mod4Mask .|. controlMask, xK_space), sendMessage ToggleLayout)
        , ((mod4Mask .|. controlMask, xK_d), sendMessage $ JumpToLayout "default")
        , ((mod4Mask .|. controlMask, xK_f), sendMessage $ JumpToLayout "fullTwoLayout")
        , ((mod4Mask .|. controlMask, xK_t), sendMessage $ JumpToLayout "three")
        -- subgroups
        , ((mod5Mask, xK_Tab), onGroup W.focusDown')
        , ((mod5Mask .|. shiftMask, xK_Tab), onGroup W.focusUp')
        , ((mod4Mask .|. controlMask, xK_Left), sendMessage $ pullGroup L)
        , ((mod4Mask .|. controlMask, xK_Right), sendMessage $ pullGroup R)
        , ((mod4Mask .|. controlMask, xK_Up), sendMessage $ pullGroup U)
        , ((mod4Mask .|. controlMask, xK_Down), sendMessage $ pullGroup D)
        , ((mod4Mask .|. shiftMask, xK_Left), sendMessage $ pushWindow L)
        , ((mod4Mask .|. shiftMask, xK_Right), sendMessage $ pushWindow R)
        , ((mod4Mask .|. shiftMask, xK_Up), sendMessage $ pushWindow U)
        , ((mod4Mask .|. shiftMask, xK_Down), sendMessage $ pushWindow D)
        -- Tab all windows in the current workspace with current window as the focus
        , ((mod4Mask .|. controlMask, xK_m), withFocused (sendMessage . MergeAll))
        -- Group the current tabbed windows
        , ((mod4Mask .|. controlMask, xK_u), withFocused (sendMessage . UnMerge))
        -- BSP's key bindings
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
        , ((mod4Mask .|. mod5Mask, xK_Left), sendMessage $ RotateL)
        , ((mod4Mask .|. mod5Mask, xK_Right), sendMessage $ RotateR)
--        , ((mod4Mask .|. controlMask, xK_Up), sendMessage $ FlipH)
--        , ((mod4Mask .|. controlMask, xK_Down), sendMessage $ FlipV)
        -- between combined layouts
        , ((mod4Mask .|. controlMask .|. shiftMask, xK_Right), sendMessage $ Move R)
        , ((mod4Mask .|. controlMask .|. shiftMask, xK_Left ), sendMessage $ Move L)
        , ((mod4Mask .|. controlMask .|. shiftMask, xK_Up   ), sendMessage $ Move U)
        , ((mod4Mask .|. controlMask .|. shiftMask, xK_Down ), sendMessage $ Move D)
        , ((mod4Mask, xK_Right), (sendMessage $ Go R))
        , ((mod4Mask, xK_Left ), (sendMessage $ Go L))
        , ((mod4Mask, xK_Up   ), (sendMessage $ Go U))
        , ((mod4Mask, xK_Down ), (sendMessage $ Go D))
        -- hidden windows
        , ((mod4Mask .|. shiftMask, xK_h), withFocused hideWindow)
        , ((mod4Mask .|. mod1Mask, xK_h), popNewestHiddenWindow)
--        , ((mod4Mask .|. shiftMask, xK_h), withFocused minimizeWindow)
--        , ((mod4Mask .|. mod1Mask, xK_h), withLastMinimized maximizeWindowAndFocus)
        , ((mod4Mask .|. shiftMask, xK_t), centerFloat)
        ]
        ++
        [((mod4Mask .|. m, k), workspaceHint f i)
        | (i, k) <- zip myWorkspaces ([xK_1 .. xK_9] ++ [xK_0] ++ [xK_F1 .. xK_F10])
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask), (copy, mod3Mask)]
        ]
        ++
        -- https://hackage.haskell.org/package/xmonad-contrib-0.16/docs/XMonad-Actions-TagWindows.html
        [((mod2Mask .|. m, k), f tag)
        | (k, tag) <- zip ([xK_0 .. xK_9] ++ [xK_a .. xK_z] ++ [xK_F1 .. xK_F12]) ((map (:[]) (['0' .. '9'] ++ ['a' .. 'z'])) ++ (map (("F"++) . (:[])) ['1' .. '9']) ++ ["F10", "F11", "F12"])
        , (f, m) <- [(withFocused . addTag, mod1Mask), (withFocused . delTag, shiftMask), (focusUpTaggedGlobal, 0)]
        ]
      )

notifyWSHint :: String -> X()
notifyWSHint index = spawn $ "notify-send -t 500 \"workspace: " ++ index ++ "\""

notifyCurrentWSHint :: X()
notifyCurrentWSHint = do
  cur <- gets (W.currentTag . windowset)
  notifyWSHint cur

workspaceHint f i = do
  windows $ f i
  notifyWSHint i

toggleWSWithHint :: X()
toggleWSWithHint = do
  toggleWS
  notifyCurrentWSHint

centerFloat = withFocused $ \f -> windows =<< appEndo `fmap` runQuery doCenterFloat f
fullFloat = withFocused $ \f -> windows =<< appEndo `fmap` runQuery doFullFloat f

myXmonadCmds =
  [ ("copyToAll"        , windows copyToAll)
  , ("keepTheCurrent"   , killAllOtherCopies)
  , ("exit", confirmPrompt myPromptConfig "Exit Xmonad?" $ io (exitWith ExitSuccess))
  , ("dvorak", spawn "setxkbmap dvorak; xmodmap ~/.Xmodmap")
  , ("centerFloat", centerFloat)
  , ("fullFloat", fullFloat)
  ]


defaultMyLayout = toggleLayouts (noBorders Full) usedLayout
usedLayout = minimize (
  hiddenWindows (
  windowNavigation (
  defaultLayout
  ||| fullTwoLayout
  ||| threeColumnLayout
  )
  )
  )

defaultLayout =
  renamed [Replace "default"] $
  addTabsBottom shrinkText tabTheme $ subLayout [] Simplest $
  emptyBSP

fullTwoLayout =
  renamed [Replace "fullTwoLayout"] $
  combineTwo (TwoPane (3/100) (1/2))
  (tabbedBottom shrinkText tabTheme)
  (tabbedBottom shrinkText tabTheme)

threeColumnLayout =
  renamed [Replace "three"] $
  addTabsBottom shrinkText tabTheme $ subLayout [] Simplest $
  ThreeColMid 1 (3/100) (3/7)
  
myGridSelectConfig = defaultGSConfig { gs_cellheight = 150, gs_cellwidth = 450 }

floatManageHook = composeAll
  [
    className =? "Xfce4-appfinder" --> doCenterFloat
  , className =? "Xfce4-settings-manager" --> doFloat
  , appName =? "emacs" --> doFloat
  , appName =? "xclock" --> doFloat
  ]

myWorkspaces = map show [1..20 :: Int]

myPromptConfig = def
  {
    font = "xft:WenQuanYi Micro Hei Mono:size=10:bold:antialias=true"
  , promptBorderWidth = 0
  , position = Bottom
  , defaultText = ""
  , alwaysHighlight = True
  , historySize = 1024
  , height = 40
  }

tabTheme = def
  {
    -- sudo dnf install wqy-microhei-fonts; fc-list | grep wqy
    fontName = "xft:WenQuanYi Micro Hei Mono:size=10:bold:antialias=true"
  , activeTextColor = "black"
  , activeColor = "#f6f5f4"
  , inactiveTextColor = "white"
  , decoHeight = 40
  }

startup :: X()
startup = do
        setWMName "LG3D"
        spawnOnce "xrdb -merge ~/.xmonad/.Xresources"
        spawnOnce "xrandr --output LVDS1 --auto; xrandr --output VGA1 --auto --right-of LVDS1"
        spawnOnce "xscreensaver -no-splash"
        spawn "arr=($(psgrep xfce4-notifyd | awk '{print $2}')); [[ ${#arr[@]} == 0 ]] && /usr/lib64/xfce4/notifyd/xfce4-notifyd"
        spawn "xfce4-panel -q; xfce4-panel -d"
        spawnOnce "nm-applet"
        spawnOnce "xfce4-power-manager"
        spawnOnce "blueberry-tray"
        spawnOnce "yong -d"

