-- base
import Control.Monad
import System.Exit

-- Hackage
import Data.Default
import qualified Data.Map as M

-- xmonad
import XMonad
import qualified XMonad.StackSet as W

-- xmonad-contrib
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.Prompt
import XMonad.Prompt.Pass
import XMonad.Prompt.Shell

-- xmonad-extra
import XMonad.Actions.Volume

-- xmonad-wallpaper
import XMonad.Wallpaper


myManageHook = composeAll
    [
      className =? "Pidgin"         --> doShift "1:chat"
    , className =? "Sylpheed"       --> doShift "1:chat"
    , className =? "Chromium"       --> doShift "2:web"
    , className =? "Google-chrome"  --> doShift "2:web"
    , className =? "Emacs"          --> doShift "3:code"
    , className =? "Kodi"           --> doShift "5:media"
    , className =? "scribus"        --> doShift "8:writing"
    , className =? "Okular"         --> doShift "7:reading"
    , className =? "VirtualBox"     --> doShift "9:vm"
    , resource  =? "desktop_window" --> doIgnore
    , className =? "Gimp"           --> doFloat
    , title     =? "pinentry-gtk-2" --> doFloat
    , resource  =? "gpicview"       --> doFloat
    , className =? "MPlayer"        --> doFloat
    , isFullscreen --> (doF W.focusDown <+> doFullFloat)]



myLayouts = avoidStruts (
    Tall 1 (3/100) (1/2)            |||
    Mirror (Tall 1 (3/100) (1/2))   |||
    tabbed shrinkText tabConfig     |||
    Full                            |||
    spiral (6/7))                   |||
    noBorders (fullscreenFull Full) |||
    layoutHook desktopConfig

tabConfig :: Theme
tabConfig = def {
    activeBorderColor   = "#7c7c7c",
    activeTextColor     = "#ceffac",
    activeColor         = "#000000",
    inactiveBorderColor = "#7c7c7c",
    inactiveTextColor   = "#eeeeee",
    inactiveColor       = "#000000"
}

myXPConfig = def
  { position          = Top
  , alwaysHighlight   = True
  , promptBorderWidth = 0
  , font              = "xft:monospace:size=9"
  }

-- Keys ------------------------------------------------------------------------

myModMask :: KeyMask
myModMask = mod4Mask

-- Apple Magic Keyboard with Numeric Keypad - US English
xK_XF86AudioMute :: KeySym
xK_XF86AudioMute = 0x1008FF12

xK_XF86XK_AudioLowerVolume :: KeySym
xK_XF86XK_AudioLowerVolume = 0x1008FF11

xK_XF86XK_AudioRaiseVolume :: KeySym
xK_XF86XK_AudioRaiseVolume = 0x1008FF13

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@XConfig {XMonad.modMask = modMask} = M.fromList $
  [
    -- launch a terminal
    ((modMask .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

   -- Lock the screen using xscreensaver.
  , ((modMask .|. controlMask, xK_l), spawn "xscreensaver-command -lock")

  -- Lower, raise, mute volume.
  , ((0, xK_XF86XK_AudioLowerVolume), void (lowerVolume 3))
  , ((0, xK_XF86XK_AudioRaiseVolume), void (raiseVolume 3))
  , ((0, xK_XF86AudioMute), void toggleMute)

  -- pass(1) prompt
  , ((modMask .|. controlMask , xK_p), passPrompt ((def XPC) { font = "xft:Fira Mono:style=Bold" }))

  --------------------------------------------------------------------
  -- "Standard" xmonad key bindings
  --

  -- Close focused window.
  , ((modMask .|. shiftMask, xK_c), kill)

  -- Cycle through the available layout algorithms.
  , ((modMask, xK_space), sendMessage NextLayout)

  --  Reset the layouts on the current workspace to default.
  , ((modMask .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)

  -- Resize viewed windows to the correct size.
  , ((modMask, xK_n), refresh)

  -- Move focus to the next window.
  , ((modMask, xK_Tab), windows W.focusDown)

  -- Move focus to the next window.
  , ((modMask, xK_j), windows W.focusDown)

  -- Move focus to the previous window.
  , ((modMask, xK_k), windows W.focusUp)

  -- Move focus to the master window.
  , ((modMask, xK_m), windows W.focusMaster)

  -- Swap the focused window and the master window.
  , ((modMask, xK_Return), windows W.swapMaster)

  -- Swap the focused window with the next window.
  , ((modMask .|. shiftMask, xK_j), windows W.swapDown)

  -- Swap the focused window with the previous window.
  , ((modMask .|. shiftMask, xK_k), windows W.swapUp)

  -- Shrink the master area.
  , ((modMask, xK_h), sendMessage Shrink)

  -- Expand the master area.
  , ((modMask, xK_l), sendMessage Expand)

  -- Push window back into tiling.
  , ((modMask, xK_t), withFocused $ windows . W.sink)

  -- Increment the number of windows in the master area.
  , ((modMask, xK_comma), sendMessage (IncMasterN 1))

  -- Decrement the number of windows in the master area.
  , ((modMask, xK_period), sendMessage (IncMasterN (-1)))

  -- Quit xmonad.
  , ((modMask .|. shiftMask .|. controlMask, xK_q), io exitSuccess)

  -- Restart xmonad
  , ((modMask, xK_q), spawn "xmonad --recompile; xmonad --restart")

  , ((modMask, xK_p), shellPrompt myXPConfig)

  ]
  ++

  --
  -- mod-[1..9], Switch to workspace N
  -- mod-shift-[1..9], Move client to workspace N
  --
  [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]


-- Mouse bindings --------------------------------------------------------------
myMouseBindings :: XConfig l -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings XConfig {XMonad.modMask = modMask} = M.fromList
  [
    -- mod-button1, Set the window to floating mode and move by dragging
    ((modMask, button1), \w -> focus w >> mouseMoveWindow w)

    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2), \w -> focus w >> windows W.swapMaster)

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w
                                          >> windows W.shiftMaster))
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
  ]

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = handleEventHook def <+> docksEventHook

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--


main :: IO ()
main = do
    setRandomWallpaper ["/void/pictures/wallpapers"]

    let workspaces = ["1:chat","2:web","3:code","4:vm","5:media","6:extra", "7:reading", "8:writing", "9:vm"]

    spawn "xmobar"

    xmonad $ desktopConfig {
        -- simple stuff
        terminal           = "konsole",
        focusFollowsMouse  = True,
        borderWidth        = 4,
        modMask            = myModMask,
        workspaces         = workspaces,
        normalBorderColor  = "#7c7c7c",
        focusedBorderColor = "#ceffac",

        -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

        -- hooks, layouts
        layoutHook      = desktopLayoutModifiers myLayouts,
        manageHook      = myManageHook <+> manageDocks <+> manageHook def,
        handleEventHook = myEventHook,
        logHook         = dynamicLogString def >>= xmonadPropLog,
        startupHook     = myStartUpHook
    }

myStartUpHook :: X ()
myStartUpHook = do
    spawn "setxkbmap -option ctrl:nocaps"
    spawn "xinput set-prop 'Logitech Trackball' 'libinput Accel Speed' 1"
    spawn "xscreensaver"
    return ()
