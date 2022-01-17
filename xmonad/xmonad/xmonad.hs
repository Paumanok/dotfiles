import XMonad
import Data.Monoid
import Data.Maybe (fromJust)
import System.Exit
import System.IO (hPutStrLn)

import XMonad.Prompt
import XMonad.Prompt.Input
import Data.Char (isSpace)
import XMonad.Actions.TreeSelect (toWorkspaces, treeselectWorkspace)

import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops

import XMonad.Util.SpawnOnce
import XMonad.Util.Run
import XMonad.Util.EZConfig (additionalKeysP)


import XMonad.Layout.Gaps
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
import XMonad.Layout.Grid

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal      = "terminator"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Width of the window border in pixels.
myBorderWidth   = 1

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
myModMask :: KeyMask
myModMask       = mod4Mask

-- Border colors for unfocused and focused windows, respectively.
myNormalBorderColor  = "#500350"
myFocusedBorderColor = "#800680"


------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys :: [(String, X ())]
--myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
myKeys =
    -- launch a terminal
    [ (("M1-<Return>"), spawn (myTerminal))

    -- launch dmenu
    --, ((modm,               xK_d     ), spawn "i3-dmenu-desktop &")
    , (("M-d"), spawn "dmenu_run -sb \"#500350\"")

    -- launch gmrun
    , (("M-S-p"), spawn "gmrun")

    -- close focused window
    , (("M-S-q"), kill)

     -- Rotate through the available layout algorithms
    , (("M-<Space>"), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    -- , (("M-S-<Space>"), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , (("M-n"), refresh)

    -- Move focus to the next window
    , (("M-<Tab>"), windows W.focusDown)

    -- Move focus to the next window
    , (("M-j"), windows W.focusDown)

    -- Move focus to the previous window
    , (("M-k"), windows W.focusUp  )

    -- Move focus to the master window
    , (("M1-m"), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , (("M1-S-<Return>"), windows W.swapMaster)
    , (("M-`"), windows W.swapMaster)

    -- Swap the focused window with the next window
    , (("M-S-j" ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , (("M-S-k"), windows W.swapUp    )

    -- Shrink the master area
    , (("M-h"), sendMessage Shrink)

    -- Expand the master area
    , (("M-l"), sendMessage Expand)

    -- Push window back into tiling
    , (("M-t"), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , (("M-,"), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , (("M-."), sendMessage (IncMasterN (-1)))

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    , (("M-S-e" ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , (("M-q"), spawn "xmonad --recompile; xmonad --restart")

    -- Run xmessage with a summary of the default keybindings (useful for beginners)
    , (("M-S-/"), spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))

    -- i3 lock screen
    , (("M1-l"), spawn "i3lock -c 000000")
    -- Sound controls
    --, ("<XF86AudioMute>", spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
    , ("<XF86AudioMute>", spawn "pulse_wrangle v m")
    --, ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ -10%")
    , ("<XF86AudioLowerVolume>", spawn "pulse_wrangle v d")
    --, ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ +10%")
    , ("<XF86AudioRaiseVolume>", spawn "pulse_wrangle v u")
    , ("C-<Page_Up>"           , spawn "pactl set-sink-volume @DEFAULT_SINK@ +10%")
    , ("C-<Page_Down>"         , spawn "pactl set-sink-volume @DEFAULT_SINK@ -10%")
    -- Screenshot  
    , ("M-S-s"           , spawn "sleep 0.2;`scrot -s -e 'xclip -selection clipboard -t image/png -i $f && rm $f'`") --throws away screenshot
    --todo save screenshot check obsidian notes
    , ("M-i"             , spawn "insync show &")

    , ("M1-="            , nviewPrompt myXPConfig "nview" )
    , ("M1-S--"            , pulse_wranglePrompt myXPConfig "pulse_wrangle" )
    , ("M1--"            , spawn "/home/matt/.local/bin/scripts/pulse_wrangle i n" )
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --adjusting normal workspace mods to use alt and letter keys
     [(("M1-" ++ m ++ k), windows $ f i)
        | (i,k) <- zip myWorkspaces xdo_workspace_keys
        , (f,m) <- [(W.greedyView, ""), (W.shift, "S-")]]
     ++
     --
     -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
     -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
     --
     [(("M-M1-" ++ key), screenWorkspace sc >>= flip whenJust (windows . f))
         | (key, sc) <- zip ["1", "2", "3"] [0..]
         , (f, m) <- [(W.view, ""), (W.shift, "S-")]]

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout = spacingWithEdge 2 $ gaps [(U,6), (D, 24), (L, 6), (R, 6)] $ avoidStruts (tiled ||| Mirror tiled ||| Full ||| simpleTabbed ||| Grid)
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

-----------------------------------------------------------------------
--XPrompt Config
myXPConfig :: XPConfig
myXPConfig = def
    { font              = "xft:Bitstream Vera Sans Mono:size=18:bold:antialias=true"
    , bgColor           = "black"
    , borderColor       = "black"
    , fgColor           = "#800680"
    , position          = CenteredAt {xpCenterY = 0.1, xpWidth = 0.3}
    , height            = 30
    }


-- nview propmpt
nviewPrompt :: XPConfig -> String -> X ()
nviewPrompt c ans =
    inputPrompt c (trim ans) ?+ \input ->
        liftIO(runProcessWithInput "/home/matt/.local/bin/scripts/nview" [input] "") >>= nviewPrompt c
    --where
    --    trim = f . f
    --        where f = reverse . dropWhile isSpace

-- pulsewranlge propmpt
pulse_wranglePrompt :: XPConfig -> String -> X ()
pulse_wranglePrompt c ans =
    inputPrompt c (trim ans) ?+ \input ->
        liftIO(runProcessWithInput "/home/matt/.local/bin/scripts/pulse_wrangle" [input] "") >>= pulse_wranglePrompt c
------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , className =? "insync"         --> doFloat
    , title =? "Media viewer"   --> doFloat --telegrams picture viewer, so it doesnt open new tile
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ]

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = mempty

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
myLogHook = return ()

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = do
    spawnOnce "nitrogen --restore &"
    spawnOnce "xsetroot -cursor_name left_ptr"
    spawnOnce "picom --config ~/.config/compton.conf &"
    spawnOnce "trayer --edge bottom --align right --widthtype request --padding 3 --SetDockType True --monitor 1 --height 17 --alpha 0 --tint 0x500350 --transparent true &"
    spawnOnce "nm-applet &"
    --spawnOnce "volumeicon &"
    spawnOnce "insync start"
    spawnOnce "dunst"

myWorkspaces    = ["Alpha", "Sierra", "Delta", "Foxtrot", "Quebec", "Whiskey", "Echo", "Romeo", "Zulu", "X-ray", "Charlie", "Victor", "Obsidian"]
workspace_keys = [xK_a, xK_s, xK_d, xK_f, xK_q, xK_w, xK_e, xK_r, xK_z, xK_x, xK_c, xK_v, xK_O]
xdo_workspace_keys = ["a","s","d","f","q","w","e","r","z","x","c","v","o"]

myWorkspaceIndices = M.fromList $ zipWith (,) myWorkspaces xdo_workspace_keys -- (,) == \x y -> (x,y)

clickable ws = "<action=xdotool key Alt_L+"++show i++">"++ws++"</action>"
    where i = fromJust $ M.lookup ws myWorkspaceIndices

--myWorkspaceIndices = M.fromList $ zipWith (,) myWorkspaces xdo_workspace_keys
--clickable ws = "<action=xdototool key Alt_L+"++show(i)++">"++ws++"</action>"
--    where i = fromJust $ M.lookup ws myWorkspaceIndices
------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main = do
    xmproc0 <- spawnPipe "xmobar -x 0 /home/matt/.config/xmobar/xmobarrc2"
    xmproc1 <- spawnPipe "xmobar -x 1 /home/matt/.config/xmobar/xmobarrc"
    xmonad $ docks $ ewmh defaults
        {    logHook = dynamicLogWithPP xmobarPP
            {    ppOutput = \x -> hPutStrLn xmproc0 x
                    >> hPutStrLn xmproc1 x
            ,   ppCurrent = xmobarColor "#800680" "" . wrap "<box type=Bottom width=2 mb=2 color=#800680>" "</box>"
            ,   ppHidden = xmobarColor "#7E7E7E" "" . clickable
            ,   ppVisible = xmobarColor "#500350" "" . clickable
            ,   ppTitle = xmobarColor "#800680" "" . shorten 60
            ,   ppSep = "<fc=#000000> <fn=1>|</fn> </fc>"
            }
        }`additionalKeysP` myKeys

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
defaults = def {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        -- keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        logHook            = myLogHook,
        startupHook        = myStartupHook
    }
-- | Finally, a copy of the default bindings in simple textual tabular format.
help :: String
help = unlines ["The default modifier key is 'alt'. Default keybindings:",
    "",
    "-- launching and killing programs",
    "mod-Shift-Enter  Launch xterminal",
    "mod-p            Launch dmenu",
    "mod-Shift-p      Launch gmrun",
    "mod-Shift-c      Close/kill the focused window",
    "mod-Space        Rotate through the available layout algorithms",
    "mod-Shift-Space  Reset the layouts on the current workSpace to default",
    "mod-n            Resize/refresh viewed windows to the correct size",
    "",
    "-- move focus up or down the window stack",
    "mod-Tab        Move focus to the next window",
    "mod-Shift-Tab  Move focus to the previous window",
    "mod-j          Move focus to the next window",
    "mod-k          Move focus to the previous window",
    "mod-m          Move focus to the master window",
    "",
    "-- modifying the window order",
    "mod-Return   Swap the focused window and the master window",
    "mod-Shift-j  Swap the focused window with the next window",
    "mod-Shift-k  Swap the focused window with the previous window",
    "",
    "-- resizing the master/slave ratio",
    "mod-h  Shrink the master area",
    "mod-l  Expand the master area",
    "",
    "-- floating layer support",
    "mod-t  Push window back into tiling; unfloat and re-tile it",
    "",
    "-- increase or decrease number of windows in the master area",
    "mod-comma  (mod-,)   Increment the number of windows in the master area",
    "mod-period (mod-.)   Deincrement the number of windows in the master area",
    "",
    "-- quit, or restart",
    "mod-Shift-q  Quit xmonad",
    "mod-q        Restart xmonad",
    "mod-[1..9]   Switch to workSpace N",
    "",
    "-- Workspaces & screens",
    "mod-Shift-[1..9]   Move client to workspace N",
    "mod-{w,e,r}        Switch to physical/Xinerama screens 1, 2, or 3",
    "mod-Shift-{w,e,r}  Move client to screen 1, 2, or 3",
    "",
    "-- Mouse bindings: default actions bound to mouse events",
    "mod-button1  Set the window to floating mode and move by dragging",
    "mod-button2  Raise the window to the top of the stack",
    "mod-button3  Set the window to floating mode and resize by dragging"]

