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
        , ((mod4Mask, xK_i), notifyCurrentWSHintWithTime)
--        , ((mod4Mask .|. shiftMask, xK_c), confirmPrompt myPromptConfig "kill window?" $ kill)
        , ((mod4Mask .|. shiftMask .|. mod1Mask, xK_b), spawn "~/.xmonad/script/toggle-xfce4-panel.sh")
        , ((mod4Mask .|. shiftMask .|. mod1Mask, xK_s), confirmPrompt myPromptConfig "Suspend?" $ spawn "systemctl suspend")
        , ((mod4Mask .|. shiftMask .|. mod1Mask, xK_Delete), confirmPrompt myPromptConfig "Lock Screen?" $ spawn "xscreensaver-command -lock")
        -- git clone https://github.com/jarun/xtrlock /opt/xtrlock; cd /opt/xtrlock; sudo make; sudo make install
        , ((mod4Mask .|. shiftMask .|. mod1Mask, xK_Escape), confirmPrompt myPromptConfig "Lock Screen?" $ spawn "xtrlock")
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
        , ((mod4Mask, xK_space), myNextLayout)
        , ((mod4Mask .|. controlMask, xK_space), sendMessage ToggleLayout)
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
        -- rebuild the BSP making the depth of the tree minimized (called balanced
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

wsHintAtIndex :: String -> X(String)
wsHintAtIndex index = do
  layout <- layoutHint
  let hint = "workspace: " ++ index ++  ", layout: " ++ layout
  return hint

notifyWSHint :: String -> Integer -> X()
notifyWSHint index interval = do
  hint <- wsHintAtIndex index
  spawn $ "notify-send -t " ++ (show interval) ++ " " ++ "\"" ++ hint ++ "\""
  
notifyWSHintWithTime :: String -> Integer -> X()
notifyWSHintWithTime index interval = do
  now <- liftIO getCurrentTime
  timezone <- liftIO getCurrentTimeZone
  hint <- wsHintAtIndex index
  let formatTimeHint = (take 19 $ show $ utcToLocalTime timezone now)
      notification =  hint ++ ", time: " ++ formatTimeHint
  spawn $ "notify-send -t " ++ (show interval) ++ " " ++ "\"" ++ notification ++ "\""

notifyCurrentWSHint interval = do
  cur <- gets (W.currentTag . windowset)
  notifyWSHint cur interval

notifyCurrentWSHintWithTime :: X()
notifyCurrentWSHintWithTime = do
  cur <- gets (W.currentTag . windowset)
  notifyWSHintWithTime cur 1500

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
  , ("dvorak", spawn "setxkbmap dvorak; xmodmap ~/.Xmodmap")
  , ("centerFloat", centerFloat)
  , ("fullFloat", fullFloat)
  ]


defaultLayout =
  toggleLayouts (noBorders Full) $
  mkToggle (single REFLECTX) $
  mkToggle (single REFLECTY) $
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
  
myGridSelectConfig = defaultGSConfig { gs_cellheight = 150, gs_cellwidth = 450 }

floatManageHook = composeAll
  [
    className =? "Xfce4-appfinder" --> doCenterFloat
  , className =? "Xfce4-settings-manager" --> doFloat
  , className =? "Thunar" --> doCenterFloat
  , className =? "Xfce4-terminal" --> doCenterFloat

  , className =? "Google-chrome" --> (doRectFloat $ (W.RationalRect (1/6) (1/6) (2/5) (2/3)))
  , className =? "Firefox" --> (doRectFloat $ (W.RationalRect (1/6) (1/6) (2/5) (2/3)))

  , className =? "Ristretto" --> doCenterFloat
  , className =? "Gimagereader-gtk" --> doCenterFloat
  , className =? "Pavucontrol" --> doCenterFloat
  , className =? "Blueberry.py" --> doCenterFloat
  , title =? "Electronic WeChat" --> doFloat
  , appName =? "emacs" --> doFloat
  , appName =? "gvim" --> doCenterFloat
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
        spawnOnce "xrandr --output LVDS1 --auto; xrandr --output VGA1 --auto --right-of LVDS1"
        spawnOnce "xscreensaver -no-splash"
        spawn "arr=($(psgrep xfce4-notifyd | awk '{print $2}')); [[ ${#arr[@]} == 0 ]] && /usr/lib64/xfce4/notifyd/xfce4-notifyd"
        spawn "xfce4-panel -q; xfce4-panel -d"
        spawnOnce "nm-applet"
        spawnOnce "xfce4-power-manager"
        -- sudo dnf install blueberry
        spawnOnce "blueberry-tray"
        spawnOnce "yong -d"

