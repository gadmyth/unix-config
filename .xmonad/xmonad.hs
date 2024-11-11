{-# OPTIONS_GHC -Wno-deprecations #-}

import Data.Char (toLower)
import Data.List (isPrefixOf)
import Data.Monoid (appEndo)
import Data.Time.Clock (getCurrentTime)
import Data.Time.LocalTime (getCurrentTimeZone, utcToLocalTime)
import XMonad hiding ( (|||) )
import XMonad.Core
import XMonad.Config.Desktop (desktopConfig, desktopLayoutModifiers)
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Util.SpawnOnce
import XMonad.Util.Themes
import XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.SetWMName
import XMonad.Layout.Grid
import XMonad.Layout.Maximize
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Reflect
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
import XMonad.Layout.SimplestFloat
import XMonad.Actions.CycleWS
import XMonad.Actions.CopyWindow
import XMonad.Actions.FloatKeys
import XMonad.Actions.GridSelect
import XMonad.Actions.GroupNavigation
import XMonad.Actions.WindowGo
import XMonad.Actions.TagWindows
import XMonad.Actions.Minimize
import XMonad.Actions.EasyMotion (EasyMotionConfig, selectWindow, cancelKey)
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.ConfirmPrompt
import XMonad.Prompt.XMonad
import Graphics.X11.ExtraTypes.XF86
import Graphics.X11.Xinerama
import System.Posix.Process
import System.IO
import System.Exit

main = do
     xmonad $ withUrgencyHook NoUrgencyHook $ ewmh desktopConfig {
          modMask = mod4Mask
          , terminal = "xfce4-terminal"
          , workspaces = myWorkspaces
          , borderWidth = 2
          , focusedBorderColor = "#ee4000"
          , normalBorderColor = "#9acd32"
          , manageHook = floatManageHook <+> manageDocks <+> manageHook desktopConfig
          , layoutHook = avoidStruts $ desktopLayoutModifiers $ defaultLayout
          , handleEventHook = handleEventHook desktopConfig <+> docksEventHook <+> fullscreenEventHook
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
        -- select to screenshot, save file to ~/Pictures directory, copy file name to clipboard
        , ((mod4Mask .|. shiftMask, xK_p), spawn "~/.xmonad/script/screenshot-and-copy-image.sh")
        -- select to screenshot, save file to ~/Pictures directory, open ~/Pictures directory
        , ((mod4Mask .|. shiftMask .|. controlMask, xK_p), spawn "~/.xmonad/script/screenshot-open-directory.sh")
        -- select to screenshot, save file to ~/Pictures directory, copy file name to clipboard
        , ((mod4Mask .|. shiftMask .|. mod1Mask, xK_p), spawn "~/.xmonad/script/screenshot-and-OCR-collect.sh")
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
        , ((mod4Mask, xK_i), notifyCurrentWSHintWithTime)
        , ((mod4Mask .|. shiftMask, xK_c), withFocused (killOrPrompt myPromptConfig))
        , ((mod4Mask .|. shiftMask .|. mod1Mask, xK_b), spawn "~/.xmonad/script/toggle-xfce4-panel.sh")
        , ((mod4Mask .|. shiftMask .|. mod1Mask, xK_s), confirmPrompt myPromptConfig "Suspend?" $ spawn "systemctl suspend")
        , ((mod4Mask .|. shiftMask .|. mod1Mask, xK_Delete), confirmPrompt myPromptConfig "Lock Screen?" $ spawn "xscreensaver-command -lock")
        -- git clone https://github.com/jarun/xtrlock /opt/xtrlock; cd /opt/xtrlock; sudo make; sudo make install
        , ((mod4Mask .|. shiftMask .|. mod1Mask, xK_Escape), confirmPrompt myPromptConfig "Lock Screen?" $ spawn "xtrlock")
        --, ((mod4Mask, xK_v), spawn "sleep 0.1; xdotool type --delay 0 \"$(xsel)\"")
        , ((mod4Mask, xK_p), spawn "xfce4-appfinder")
        , ((mod4Mask, xK_g), goToSelected myGridSelectConfig)
        -- audio
        , ((0, xF86XK_AudioRaiseVolume), spawn "~/.xmonad/script/adjust-volume.sh '+5%' true")
        , ((shiftMask, xF86XK_AudioRaiseVolume), spawn "~/.xmonad/script/adjust-volume.sh '+20%' true")
        , ((0, xF86XK_AudioLowerVolume), spawn "~/.xmonad/script/adjust-volume.sh '-5%' true")
        , ((shiftMask , xF86XK_AudioLowerVolume), spawn "~/.xmonad/script/adjust-volume.sh '-20%' true")
        , ((0, xF86XK_AudioMute), spawn "~/.xmonad/script/toggle-volume.sh true")
        -- layouts
        , ((mod4Mask, xK_space), myNextLayout)
        , ((mod4Mask .|. controlMask, xK_space), sendMessage ToggleLayout)
        -- , ((mod4Mask .|. shiftMask, xK_space), sendMessage $ XMonad.Layout.MultiToggle.Toggle NBFULL)
        , ((mod4Mask .|. controlMask, xK_1), myJumpToLayout "main")
        , ((mod4Mask .|. controlMask, xK_2), myJumpToLayout "fullTwoLayout")
        , ((mod4Mask .|. controlMask, xK_3), myJumpToLayout "three")
        , ((mod4Mask .|. controlMask, xK_4), myJumpToLayout "SimplestFloat")
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
        -- should focus the parent first, it balance the children of the tree,
        -- rebuild the BSP making the depth of the tree minimized (called balanced)
        , ((mod4Mask .|. mod1Mask, xK_b), sendMessage Balance)
        -- should focus the parent first, it equalize the children of the tree,
        -- keep the BSP's structure, make each window gets the same amount of space
        , ((mod4Mask .|. mod1Mask, xK_e), sendMessage Equalize)
        , ((mod4Mask .|. mod1Mask, xK_n), sendMessage SelectNode)
        , ((mod4Mask .|. mod1Mask, xK_m), sendMessage MoveNode)
        , ((mod4Mask .|. mod1Mask, xK_Left), sendMessage $ MoveSplit L)
        , ((mod4Mask .|. mod1Mask, xK_Right), sendMessage $ MoveSplit R)
        , ((mod4Mask .|. mod1Mask, xK_Up), sendMessage $ MoveSplit U)
        , ((mod4Mask .|. mod1Mask, xK_Down), sendMessage $ MoveSplit D)
        , ((mod5Mask, xK_Left), sendMessage $ RotateL)
        , ((mod5Mask, xK_Right), sendMessage $ RotateR)
        , ((mod4Mask .|. controlMask, xK_s), sendMessage $ XMonad.Layout.MultiToggle.Toggle REFLECTX)
        , ((mod4Mask .|. controlMask .|. shiftMask, xK_s), sendMessage $ XMonad.Layout.MultiToggle.Toggle REFLECTY)
        -- float windows
        -- https://hackage.haskell.org/package/xmonad-contrib-0.17.0/docs/XMonad-Actions-FloatKeys.html
        , ((mod4Mask, xK_h), withFocused (keysMoveWindow (-100, 0)))
        , ((mod4Mask, xK_l), withFocused (keysMoveWindow (100, 0)))
        , ((mod4Mask, xK_j), withFocused (keysMoveWindow (0, 100)))
        , ((mod4Mask, xK_k), withFocused (keysMoveWindow (0, -100)))
        , ((mod4Mask .|. mod1Mask .|. shiftMask, xK_h), withFocused (keysResizeWindow (-100, 0) (0, 0)))
        , ((mod4Mask .|. mod1Mask .|. shiftMask, xK_l), withFocused (keysResizeWindow (100, 0) (0, 0)))
        , ((mod4Mask .|. mod1Mask .|. shiftMask, xK_j), withFocused (keysResizeWindow (0, 100) (0, 0)))
        , ((mod4Mask .|. mod1Mask .|. shiftMask, xK_k), withFocused (keysResizeWindow (0, -100) (0, 0)))
        -- https://hackage.haskell.org/package/xmonad-contrib-0.17.0/docs/XMonad-Hooks-Place.html
        , ((mod4Mask .|. mod1Mask, xK_h), withFocused $ borderMove LEFT)
        , ((mod4Mask .|. mod1Mask, xK_l), withFocused $ borderMove RIGHT)
        , ((mod4Mask .|. mod1Mask, xK_j), withFocused $ borderMove BOTTOM)
        , ((mod4Mask .|. mod1Mask, xK_k), withFocused $ borderMove TOP)
        , ((mod4Mask .|. mod1Mask, xK_t), withFocused $ borderMove HORI_CENTER)
        , ((mod4Mask .|. mod1Mask, xK_v), withFocused $ borderMove VERT_CENTER)
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
        , ((mod4Mask .|. mod1Mask, xK_c), withFocused hideWindow)
        , ((mod4Mask .|. mod1Mask, xK_u), popNewestHiddenWindow)
--        , ((mod4Mask .|. shiftMask, xK_h), withFocused minimizeWindow)
--        , ((mod4Mask .|. mod1Mask, xK_h), withLastMinimized maximizeWindowAndFocus)
        , ((mod4Mask, xK_backslash), withFocused (sendMessage . maximizeRestore))
        , ((mod4Mask .|. mod1Mask, xK_t), centerFloat)
        , ((mod4Mask, xK_f), selectWindow easyMotionConf >>= (`whenJust` windows . W.focusWindow))
        ]
        ++
        [ ((mod4Mask .|. mod, key), workspaceHint func index)
        | (index, key) <- zip
          myWorkspaces ([xK_1 .. xK_9] ++ [xK_0] ++ [xK_F1 .. xK_F12])
        , (mod, func) <- [(0, W.greedyView), (shiftMask, W.shift), (mod3Mask, copy)]
        ]
        ++
        -- https://hackage.haskell.org/package/xmonad-contrib-0.16/docs/XMonad-Actions-TagWindows.html
        [ ((mod2Mask .|. mod, key), func tag)
        | (key, tag) <- zip
          ([xK_0 .. xK_9] ++ [xK_a .. xK_z] ++ [xK_F1 .. xK_F12])
          ((map (:[]) (['0' .. '9'] ++ ['a' .. 'z'])) ++ (map (("F"++) . (:[])) ['1' .. '9']) ++ ["F10", "F11", "F12"])
        , (mod, func) <- [(0, focusUpTaggedGlobal), (mod1Mask, withFocused . addTag), (shiftMask, withFocused . delTag)]
        ]
        ++
        [ ((mod4Mask .|. mod2Mask, xK_l ), tagPrompt def (\s -> focusUpTaggedGlobal s))
        ]
      )

-- https://xmonad.github.io/xmonad-docs/xmonad-contrib/XMonad-Actions-EasyMotion.html
easyMotionConf::EasyMotionConfig
easyMotionConf = def { cancelKey = xK_Escape }

killOrPrompt conf w = do
  copies <- wsContainingCopies
  if (length copies) == 0
    then confirmPrompt conf "Kill the only window?" $ kill1
    else kill1

wsHintAtIndex :: String -> X(String)
wsHintAtIndex index = do
  layout <- layoutHint
  let hint = "workspace: " ++ index ++  ", layout: " ++ layout
  return hint

notifyWSHint :: String -> Integer -> X()
notifyWSHint index interval = do
  hint <- wsHintAtIndex index
  spawn $ "~/.xmonad/script/show-workspace.sh " ++ (show interval) ++ " " ++ "\"" ++ hint ++ "\""
  
notifyWSHintWithTime :: String -> Integer -> X()
notifyWSHintWithTime index interval = do
  now <- liftIO getCurrentTime
  timezone <- liftIO getCurrentTimeZone
  hint <- wsHintAtIndex index
  let formatTimeHint = (take 19 $ show $ utcToLocalTime timezone now)
      notification =  hint ++ ", time: " ++ formatTimeHint
  spawn $ "~/.xmonad/script/show-workspace.sh " ++ (show interval) ++ " " ++ "\"" ++ notification ++ "\""

notifyCurrentWSHint interval = do
  cur <- gets (W.currentTag . windowset)
  notifyWSHint cur interval

notifyCurrentWSHintWithTime :: X()
notifyCurrentWSHintWithTime = do
  cur <- gets (W.currentTag . windowset)
  notifyWSHintWithTime cur 3000

workspaceHint f i = do
  windows $ f i
  notifyWSHint i 500

layoutHint :: X String
layoutHint = do
  workspaces <- gets windowset
  let desc = description . W.layout . W.workspace . W.current $ workspaces
      hint = last $ split ' ' desc
  return hint

toggleWSWithHint :: X()
toggleWSWithHint = do
  toggleWS
  notifyCurrentWSHint 500

myJumpToLayout :: String -> X()
myJumpToLayout name = do
  sendMessage $ JumpToLayout name
  notifyCurrentWSHint 500

myNextLayout :: X()
myNextLayout = do
  sendMessage NextLayout
  notifyCurrentWSHint 500

centerFloat = withFocused $ \f -> windows =<< appEndo `fmap` runQuery doCenterFloat f
fullFloat = withFocused $ \f -> windows =<< appEndo `fmap` runQuery doFullFloat f

myXmonadCmds =
  [ ("copyToAll"        , windows copyToAll)
  , ("keepTheCurrent"   , killAllOtherCopies)
  , ("exit", confirmPrompt myPromptConfig "Exit Xmonad?" $ io (exitWith ExitSuccess))
  , ("dvorak", spawn "~/dvorak.sh")
  , ("centerFloat", centerFloat)
  , ("fullFloat", fullFloat)
  , ("screenshot and OCR", spawn "~/.xmonad/script/screenshot-and-OCR.sh")
  , ("start emacs-28", spawn "SNAP=1 SNAP_NAME=1 SNAP_REVISION=1 /opt/emacs-28/usr/bin/emacs")
  ]


defaultLayout =
  smartBorders $
  toggleLayouts (noBorders Full) $
  mkToggle (single REFLECTX) $
  mkToggle (single REFLECTY) $
  mkToggle (NBFULL ?? NOBORDERS ?? EOT) $
  maximize $
  minimize $
  hiddenWindows $
  windowNavigation $
  mainLayout
  ||| simplestFloat
  ||| fullTwoLayout
  ||| threeColumnLayout

mainLayout =
  renamed [Replace "main"] $
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
  
myGridSelectConfig = def { gs_cellheight = 150, gs_cellwidth = 450 }

hasPrefixIgnoreCase :: String -> String -> Bool
hasPrefixIgnoreCase pattern str = map toLower pattern `isPrefixOf` map toLower str

hasPrefixIgnoreCaseQ :: Query String -> String -> Query Bool
hasPrefixIgnoreCaseQ queryStr pattern = do
    str <- queryStr
    return (hasPrefixIgnoreCase pattern str)

-- use linux command xprop to get window's class name
floatManageHook = composeAll
  [
    className =? "Xfce4-appfinder" --> doCenterFloat
  , className =? "Xfce4-settings-manager" --> doFloat
  , className =? "Thunar" --> doCenterFloat
  , className =? "Xfce4-terminal" --> doCenterFloat

  , className =? "Google-chrome" --> (doRectFloat $ (W.RationalRect (1/6) (1/6) (2/5) (2/3)))
  , className =? "Firefox" --> (doRectFloat $ (W.RationalRect (1/6) (1/6) (2/5) (2/3)))

  , className =? "Ristretto" --> doCenterFloat
  , className =? "Viewnior" --> doCenterFloat
  , className =? "Gimagereader-gtk" --> doCenterFloat
  , className =? "Pavucontrol" --> doCenterFloat
  , className =? "Blueberry.py" --> doCenterFloat
  , appName =? "blueman-manager" --> doCenterFloat
  , title =? "Electronic WeChat" --> doFloat
  , appName `hasPrefixIgnoreCaseQ` "emacs" --> doFloat
  , appName =? "gvim" --> doCenterFloat
  , appName =? "NotepadNext" --> doCenterFloat
  , appName =? "xclock" --> doFloat
  ]

myWorkspaces = map show [1..22 :: Int]

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

-- copied and modified from
-- https://hackage.haskell.org/package/xmonad-contrib-0.17.0/docs/src/XMonad.Actions.FloatSnap.html
data Place2D = TOP
             | BOTTOM
             | HORI_CENTER
             | RIGHT
             | LEFT
             | VERT_CENTER
             deriving (Eq,Read,Show,Ord,Enum,Bounded)

borderMove :: Place2D -> Window -> X ()
borderMove LEFT        = doBorderMove True 0.0
borderMove HORI_CENTER = doBorderMove True 0.5
borderMove RIGHT       = doBorderMove True 1.0
borderMove TOP         = doBorderMove False 0.0
borderMove VERT_CENTER = doBorderMove False 0.5
borderMove BOTTOM      = doBorderMove False 1.0

doBorderMove :: Bool -> Rational -> Window -> X ()
doBorderMove horiz ratio w = whenX (isClient w) $ withDisplay $ \d -> do
  wa <- io $ getWindowAttributes d w
  sa <- io $ getScreenInfo d
  let (ww, wh) = (toRational $ wa_width wa, toRational $ wa_height wa)
      (sw, sh) = (toRational $ rect_width $ sa !! 0, toRational $ rect_height $ sa !! 0)
      (dw, dh) = (sw - ww, sh - wh)
      (x_pos, y_pos) = (dw * ratio, dh * ratio)
      newpos = if horiz
        then x_pos
        else y_pos

  if horiz
    then io $ moveWindow d w (fromIntegral (floor newpos)) (fromIntegral $ wa_y wa)
    else io $ moveWindow d w (fromIntegral $ wa_x wa) (fromIntegral (floor newpos))
    
  float w

startup :: X()
startup = do
        setWMName "LG3D"
        spawnOnce "xrdb -merge ~/.xmonad/.Xresources"
        spawnOnce "~/.xmonad/script/monitor-config.sh"
        spawnOnce "~/.xmonad/script/wallpaper-config.sh"
        spawnOnce "xscreensaver -no-splash"
        spawn "~/.xmonad/script/start-xfce4-notifyd.sh"
        spawn "~/.xmonad/script/start-xfce4-panel.sh"
        spawn "~/.xmonad/script/start-xfce4-clipman.sh"
        spawn "~/dvorak.sh"
        spawnOnce "nm-applet"
        spawnOnce "xfce4-power-manager"
        -- sudo dnf install blueberry
        -- spawnOnce "blueberry-tray"
        spawnOnce "blueman-applet"
        spawnOnce "yong -d"
        spawn "notify-send -t 1500 \"Restart Xmonad Success!\""
