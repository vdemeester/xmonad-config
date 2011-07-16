import XMonad
import XMonad.Config.Kde
-- Import for ToggleStruts
import XMonad.Hooks.ManageDocks
-- Import for smartBorder
import XMonad.Layout.NoBorders
-- Tests
import XMonad.Layout.CenteredMaster
import XMonad.Layout.Grid
import XMonad.Layout.Column
import XMonad.Layout.Cross
import XMonad.Layout.Tabbed
import XMonad.Prompt
import XMonad.Prompt.Man
import XMonad.Prompt.Shell
import XMonad.Prompt.Ssh
import XMonad.Prompt.Theme
import XMonad.Prompt.XMonad

import qualified XMonad.StackSet as W -- to shift and float windows
import qualified Data.Map as M
 
main = xmonad $ kde4Config
 { modMask    = mod4Mask -- use the Windows button as mod
 , manageHook = manageHook kde4Config <+> myManageHook
 , layoutHook = myLayout
 , keys       = myKeys
 }
 where
   myManageHook = composeAll . concat $
     [ [ className   =? c --> doFloat           | c <- myFloats]
     , [ title       =? t --> doFloat           | t <- myOtherFloats]
     , [ className   =? c --> doF (W.shift "2") | c <- webApps]
     , [ className   =? c --> doF (W.shift "3") | c <- ircApps]
     ]
   myFloats      = ["MPlayer", "Gimp", "Plasma", "Plasma-desktop", "krunner"]
   myOtherFloats = ["alsamixer", "Plasma", "Plasma-desktop", "krunner"]
   webApps       = ["Firefox-bin", "Opera"] -- open on desktop 2
   ircApps       = ["Ksirc"]                -- open on desktop 3

myLayout = avoidStruts $ standardLayouts
    
    where
        -- standard layouts
        standardLayouts = smartBorders $ Mirror tiled ||| full ||| tiled ||| misc
        
        tiled = Tall 1 (2/100) (4/5)
        full  = Full
        misc  = Grid ||| Column 1.6 ||| cross ||| simpleTabbed
        cross = Cross (19/20) (1/100)

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
    , ((modm, xK_space), sendMessage NextLayout)
    , ((modm .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)
    , ((modm, xK_Return), windows W.swapMaster)
    , ((modm, xK_n), refresh)
    , ((modm, xK_Tab), windows W.focusDown)
    , ((modm, xK_t), windows W.focusDown)
    , ((modm, xK_s), windows W.focusUp)
    , ((modm, xK_m), windows W.focusMaster)
    , ((modm .|. shiftMask, xK_t), windows W.swapDown)
    , ((modm .|. shiftMask, xK_s), windows W.swapUp)
    , ((modm, xK_c), sendMessage Shrink)
    , ((modm, xK_r), sendMessage Expand)
    , ((modm, xK_i), focusScreen 1)
    , ((modm, xK_e), focusScreen 0)
    , ((modm, xK_j), spawn "dmenu_run")
    , ((modm, xK_b), sendMessage ToggleStruts)
    , ((modm, xK_F1), manPrompt defaultXPConfig)
    , ((modm .|. controlMask, xK_x), shellPrompt defaultXPConfig)
    , ((modm .|. controlMask, xK_s), sshPrompt defaultXPConfig)
    , ((modm .|. controlMask, xK_t), themePrompt defaultXPConfig)
    , ((modm .|. controlMask, xK_y), xmonadPrompt defaultXPConfig)
    -- Killing
    , ((modm .|. shiftMask, xK_c), kill)
    -- Restarting
    , ((modm, xK_q), spawn $ "xmonad --recompile && xmonad --restart")
    -- Push window back into tiling
    , ((modm, xK_l), withFocused $ windows . W.sink)
    , ((modm, xK_comma), sendMessage (IncMasterN 1))
    , ((modm, xK_period), sendMessage (IncMasterN (-1)))
    ]
    ++
    [((m .|. modm, k), windows $ f i)
        | (i,k) <- zip (XMonad.workspaces conf) [ xK_quotedbl, xK_less, xK_greater, xK_parenleft, xK_parenright, xK_at, xK_plus, xK_minus, xK_slash ]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    
    where

        focusScreen n = screenWorkspace n >>= flip whenJust (windows . W.view)
