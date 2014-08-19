{-# LANGUAGE FlexibleContexts #-}

import XMonad
import XMonad.Config.Kde
import qualified XMonad.StackSet as W -- to shift and float windows
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops
import qualified Data.Map as M
import XMonad.Actions.CycleWS
import XMonad.Actions.CycleRecentWS
import XMonad.Util.Run
import XMonad.Actions.CopyWindow
import XMonad.Actions.TopicSpace
import System.Exit
import System.Environment
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Maximize
import XMonad.Actions.SpawnOn
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Ssh
import XMonad.Prompt.Window
import XMonad.Util.NamedScratchpad
import XMonad.Hooks.ManageHelpers
import XMonad.Util.NamedWindows
import System.Taffybar.Hooks.PagerHints (pagerHints)

import qualified XMonad.StackSet as W

--- Variables {{
myTerminal = "urxvt"

-- colors
myFgColor       = "#888888"
myFgHLight      = "#FFFFFF"
myFgDimLight    = "#505050"
myTextHLight    = "#FAFAC0"
myNotifyColor   = "#F39D21"
myWarningColor  = "#D23D3D"

myBgColor       = "#181512"
myBgDimLight    = "#333333"
myBgHLight      = "#4C7899"
myBorderColor   = "#222222"
myBorderHLight  = "#285577"

myFont          = "xft:Ubuntu Mono:medium:pixelsize=14"

myXPConfig :: XPConfig
myXPConfig = defaultXPConfig
             { font        = myFont
             , bgColor     = myBgColor
             , fgColor     = myFgColor
             , fgHLight    = myFgHLight
             , bgHLight    = myBgHLight
             , borderColor = myBorderColor
             , position    = Top
             }

myWindowXPConfig = myXPConfig
                { bgColor  = "#ffdead"
                , fgColor  = "#666666"
                }
                
mySshXPConfig = myXPConfig
                { bgColor  = "#4682b4"
                , fgColor  = "#eeeeee"
                }

--- }}
--- Keys {{
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm, xK_space), sendMessage NextLayout)
    , ((modm .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)
    , ((modm, xK_Return), spawnHere myTerminal)
    , ((modm .|. controlMask, xK_l), spawnHere "screenlock")
    -- , ((mod1Mask,xK_s), shellPromptHere myXPConfig)
    -- , ((modm .|. controlMask, xK_space), myLayoutPrompt)
    , ((modm, xK_o), shellPrompt myXPConfig) -- shellPromptHere
    , ((modm .|. controlMask, xK_s), sshPrompt mySshXPConfig)
     , ((modm .|. shiftMask, xK_g), windowPromptGoto
                                            myWindowXPConfig { autoComplete = Just 500000 } )
    , ((modm .|. shiftMask, xK_f), sendMessage $ Toggle NBFULL)
    , ((modm .|. mod1Mask, xK_Return), windows W.swapMaster)
    , ((modm .|. controlMask, xK_n), refresh)
    , ((modm, xK_dollar), namedScratchpadAction scratchpads "scratch")
    , ((modm, xK_Tab), windows W.focusDown)
    , ((modm, xK_t), windows W.focusDown)
    , ((modm, xK_s), windows W.focusUp)
    , ((modm, xK_m), windows W.focusMaster)
    , ((modm .|. shiftMask, xK_m), withFocused (sendMessage . maximizeRestore))
    , ((modm .|. shiftMask, xK_t), windows W.swapDown)
    , ((modm .|. shiftMask, xK_s), windows W.swapUp)
    , ((modm, xK_c), sendMessage Shrink)
    , ((modm, xK_r), sendMessage Expand)
    , ((modm, xK_b), sendMessage ToggleStruts)
    -- CycleWS
    , ((modm, xK_Right),  nextWS)
    , ((modm, xK_Left),    prevWS)
--    , ((modm,               xK_Down),  windows . W.greedyView =<< findWorkspace getSortByIndexNoSP Next HiddenNonEmptyWS 1)
--    , ((modm,               xK_Up),    windows . W.greedyView =<< findWorkspace getSortByIndexNoSP Prev HiddenNonEmptyWS 1)
    , ((modm .|. shiftMask, xK_Right),  shiftToNext)
    , ((modm .|. shiftMask, xK_Left),    shiftToPrev)
    , ((modm,               xK_Up), nextScreen)
    , ((modm,               xK_Down),  prevScreen)
    , ((modm .|. shiftMask, xK_Up), shiftNextScreen)
    , ((modm .|. shiftMask, xK_Down),  shiftPrevScreen)
    , ((modm,               xK_z),     toggleWS)
    -- Topics
    -- Killing
    , ((modm .|. shiftMask, xK_x), kill)
    -- Urgent !!!
    , ((modm, xK_y), focusUrgent)
    -- Restarting
    , ((modm .|. controlMask .|. shiftMask, xK_q),           io (exitWith ExitSuccess))
    -- Push window back into tiling
    , ((modm, xK_l), withFocused $ windows . W.sink)
    , ((modm, xK_comma), sendMessage (IncMasterN 1))
    , ((modm, xK_period), sendMessage (IncMasterN (-1)))
    ]
    -- FIXME: the following stuff is doing no shit
    ++ [((m .|. modm, k), windows $ f i)
       | (i, k) <- zip (XMonad.workspaces conf) [ xK_quotedbl, xK_less, xK_greater, xK_parenleft, xK_parenright, xK_at, xK_plus, xK_minus, xK_slash ]
       , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

---    [((modm, k), switchNthLastFocused myTopicConfig i)
---        | (i,k) <- zip [1..9] [ xK_quotedbl, xK_less, xK_greater, xK_parenleft, xK_parenright, xK_at, xK_plus, xK_minus, xK_slash ]
---    ]
isModal :: Query Bool
isModal = isInProperty "_NET_WM_STATE" "_NET_WM_STATE_MODAL"

--- }}
--- Scratchpads {{
scratchpads :: [NamedScratchpad]
scratchpads =
    [NS "scratch" "urxvt -title sp-term -name sp-term -e tmx default" (resource =? "sp-term")
        (customFloating $ W.RationalRect 0 0 1 0.4)
    ]
--- }}
--- Main {{
main = xmonad $ pagerHints $ kdeConfig
    {
      terminal = myTerminal
    , keys = myKeys
    , startupHook = ewmhDesktopsStartup <+> setWMName "LG3D"
    , focusFollowsMouse = False
    , modMask = mod4Mask -- use the Windows button as mod
    , manageHook = composeAll
--      [ manageSpawn <+> manageHook kdeConfig <+> myManageHook
      [ isFullscreen --> doFullFloat
      , isModal --> doFloat
      , manageSpawn
      , namedScratchpadManageHook scratchpads
      , manageHook kdeConfig
      , myManageHook
      ]
    }
 
myManageHook = composeAll . concat $
    [ [ className   =? c --> doFloat           | c <- myFloats]
    , [ title       =? t --> doFloat           | t <- myOtherFloats]
    , [ className   =? c --> doF (W.shift "2") | c <- webApps]
    , [ className   =? c --> doF (W.shift "3") | c <- ircApps]
    ]
  where myFloats      = ["MPlayer", "Gimp", "Plasma-desktop"]
        myOtherFloats = ["alsamixer"]
        webApps       = ["Firefox-bin", "Opera"] -- open on desktop 2
        ircApps       = ["Ksirc"]                -- open on desktop 3
--- }}
