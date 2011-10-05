-------------------------------------------------------------------------------
-- |
-- Module       :   xmonad.hs
-- Copyright    :   (c) Vincent Demeester
-- License      :   as-is
--
-- Maintainer   :   vincent+xmonad AT demeester.fr
-- Stability    :   unstable
-- Portability  :   unportable (but want to be)
--
-- Up to date xmonad configuration, using xmonad-0.9.2 packaged on Debian 
-- testing (wheezy)
--
-- vim:foldmethod=marker foldmarker={{{,}}}
-------------------------------------------------------------------------------

-- Imports {{{
import XMonad
-- Actions
import XMonad.Actions.CycleWS
import XMonad.Actions.SpawnOn
import XMonad.Actions.Submap
import XMonad.Actions.Search
-- Hooks
-- Import for ToggleStruts
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (doCenterFloat)
-- Layout
-- Import for smartBorder
import XMonad.Layout.NoBorders
-- Tests
import XMonad.Layout.Grid
import XMonad.Layout.Tabbed
-- Prompt(s)
import XMonad.Prompt
import XMonad.Prompt.Man
import XMonad.Prompt.RunOrRaise
import XMonad.Prompt.Shell
import XMonad.Prompt.Ssh
import XMonad.Prompt.Workspace
-- Topics
import XMonad.Actions.TopicSpace
-- Util
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Run
-- Misc
import qualified XMonad.StackSet as W -- to shift and float windows
import qualified Data.Map as M
import Data.Monoid(mconcat)
-- Local import
import ScratchPadKeys
import Utils
-- }}}

--- Main {{{
main = do
    checkTopicConfig myTopics myTopicConfig
    xmproc <- spawnPipe "xmobar"
    sp <- mkSpawner
    xmonad $ defaultConfig
        { terminal              = myTerminal
        , workspaces            = myTopics
        , modMask               = mod4Mask -- use the Windows button as mod
        , manageHook            = manageHook defaultConfig <+> manageSpawn sp <+> myManageHook <+> manageScratchPads myScratchPadList
        , layoutHook            = myLayout
        , logHook               = dynamicLogWithPP $ myPP { ppOutput = hPutStrLn xmproc }
        , keys                  = myKeys sp
        , focusFollowsMouse     = False
        , focusedBorderColor    = base0
        , normalBorderColor     = base2
        } `additionalKeysP` myAdditionalKeys
--- }}}

--- Options {{{
-- Misc.
myTerminal          = "urxvtc"
myShell             = "bash"
myBorderWidth       = 1
-- Fonts (xft :-) & co)
barFont             = "Play-9"
themeFont           = "xft:Play:size=9"
-- Colors, solarized
base03              = "#002b36"
base02              = "#073642"
base01              = "#586e75"
base00              = "#657b83"
base0               = "#839496"
base1               = "#93a1a1"
base2               = "#eee8d5"
base3               = "#fdf6e3"
yellow              = "#b58900"
orange              = "#cb4b16"
red                 = "#dc322f"
magenta             = "#d33682"
violet              = "#6c71c4"
blue                = "#268bd2"
cyan                = "#2aa198"
green               = "#859900"
-- Search engines {{{
-- Custom engine definition (and custom search funciton ?)

-- Map
searchEngineMap method = M.fromList $
    [ ((0, xK_g), method google)
    , ((0, xK_h), method hoogle)
    , ((0, xK_w), method wikipedia)
    , ((0, xK_d), method deb)
    , ((0, xK_b), method debbts)
    ]
--- }}}
--- }}}

--- Themes {{{
-- Theme for prompt
myXPConfig = defaultXPConfig                                    
    { font      = themeFont
    , fgColor   = base03
    , bgColor   = base3
    , bgHLight  = base2
    , fgHLight  = blue
    , position  = Top
    }
 
--- Theme For Tabbed layout
myTheme = defaultTheme 
    { fontName              = themeFont
    , decoHeight            = 14
    , activeColor           = base03
    , activeBorderColor     = base02
    , activeTextColor       = base3
    , inactiveColor         = base0
    , inactiveBorderColor   = base1
    , inactiveTextColor     = base2
    , urgentColor           = yellow
    , urgentBorderColor     = red
    , urgentTextColor       = red
    }
--- }}}

--- DynamicLogs {{{
myPP :: PP
myPP = xmobarPP
    { ppHidden = hideNSP
    , ppTitle   = xmobarColor base01 "" . shorten 100
    , ppCurrent = xmobarColor base3 base00 . pad
    , ppSep     = xmobarColor base00 "" " "
    , ppUrgent  = xmobarColor base3 red . xmobarStrip
    , ppLayout  = xmobarColor base2 blue . pad . \s ->
        case s of
            "Mirror Tall"          -> "Tall"
            "Tabbed Simplest"      -> "Tab"
            _                      -> pad s
    }
--- }}}

--- Topics (Workspaces) {{{
-- Topic definition, lots of !
myTopics :: [Topic]
myTopics =
    [ "default" -- the default one
    , "web", "mail"
    , "config", "xmonad", "haskell" -- dev
    , "sbr.org", "sites" -- sites
    , "music", "video", "pictures" -- multimedia
    , "irc", "chat" -- chat
    , "doc", "ebook" -- documents
    , "test", "wip"
    ]
-- Topic configuration
myTopicConfig :: TopicConfig
myTopicConfig = TopicConfig
    { topicDirs = M.fromList $
        [ ("config", "src/git/configs")
        , ("xmonad", "src/git/configs/xmonad")
        , ("sites", "src/git/sites")
        , ("sbr.org", "src/git/sites/shortbrain.org")
        , ("music", "music")
        , ("video", "video")
        , ("pictures", "pictures")
        , ("doc", "documents")
        , ("ebook", "documents/ebook")
        ]
    , defaultTopicAction = const $ spawnShell
    , defaultTopic = "default"
    , topicActions = M.fromList $
        [ ("config", spawnShell)
        , ("xmonad", spawnShell >> spawn "cd .xmonad && gvim xmonad.hs")
        , ("music", spawn $ myTerminal ++ " -e ncmpcpp")
        ]
    }

goto :: Topic -> X ()
goto = switchTopic myTopicConfig

promptedGoto :: X ()
promptedGoto = workspacePrompt myXPConfig goto

promptedShift :: X ()
promptedShift = workspacePrompt myXPConfig $ windows . W.shift

spawnDolphin :: X ()
spawnDolphin = currentTopicDir myTopicConfig >>= spawnDolphinIn
 
spawnDolphinIn :: Dir -> X ()
spawnDolphinIn dir = spawn $ "sh -c 'cd ''" ++ dir ++ "'' && dolphin .'"

spawnShell :: X ()
spawnShell = currentTopicDir myTopicConfig >>= spawnShellIn
 
spawnShellIn :: Dir -> X ()
spawnShellIn dir = spawn $ myTerminal ++ " -title urxvt -e sh -c 'cd ''" ++ dir ++ "'' && " ++ myShell ++ "'"
--- }}}

--- ManageHooks {{{
myManageHook :: ManageHook
myManageHook = composeAll [ matchAny v --> a | (v,a) <- myActions ] -- <+> manageScratchPads scratchPadList

    where myActions = [ ("rdesktop"  , doFloat         )
                      , ("Xmessage"  , doCenterFloat   )
                      , ("Iceweasel" , doShift "web" )
                      , ("Firefox"   , doShift "web" )
                      , ("irssi"     , doShift "irc")
                      ]
--- }}}

--- Layout {{{
myLayout = avoidStruts $ standardLayouts
    
    where
        -- standard layouts
        standardLayouts = smartBorders $ Mirror tiled ||| full ||| tiled ||| misc
        
        tiled = Tall 1 (2/100) (4/5)
        full  = Full
        misc  = Grid ||| tabbed shrinkText myTheme
--- }}}

--- Keys {{{
myKeys sp conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm .|. controlMask, xK_Return), spawnHere sp $ XMonad.terminal conf)
    , ((modm .|. shiftMask, xK_Return), spawnShell) -- start shell in topic dir
    , ((modm, xK_d), spawnDolphin) -- start shell in topic dir
    --[ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
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
    , ((modm, xK_b), sendMessage ToggleStruts)
    -- Prompt(s)
    , ((modm, xK_F1), manPrompt myXPConfig)
    , ((modm, xK_j), shellPromptHere sp myXPConfig)
    , ((modm .|. shiftMask, xK_j), runOrRaisePrompt myXPConfig)
    , ((modm .|. controlMask, xK_s), sshPrompt myXPConfig)
    -- Prompt(s) search
    , ((modm, xK_F2), submap $ searchEngineMap $ promptSearch myXPConfig)
    , ((modm .|. shiftMask, xK_F2), submap $ searchEngineMap $ selectSearch)
    -- CycleWS
    , ((modm,               xK_Down),  nextWS)
    , ((modm,               xK_Up),    prevWS)
    , ((modm .|. shiftMask, xK_Down),  shiftToNext)
    , ((modm .|. shiftMask, xK_Up),    shiftToPrev)
    , ((modm,               xK_Right), nextScreen)
    , ((modm,               xK_Left),  prevScreen)
    , ((modm .|. shiftMask, xK_Right), shiftNextScreen)
    , ((modm .|. shiftMask, xK_Left),  shiftPrevScreen)
    , ((modm,               xK_z),     toggleWS)
    -- Topics
    , ((modm, xK_a), currentTopicAction myTopicConfig)
    , ((modm, xK_g), promptedGoto)
    , ((modm .|. shiftMask, xK_g), promptedShift)
    -- Killing
    , ((modm .|. shiftMask, xK_c), kill)
    -- Restarting
    , ((modm, xK_q), cleanStart)
    -- Push window back into tiling
    , ((modm, xK_l), withFocused $ windows . W.sink)
    , ((modm, xK_comma), sendMessage (IncMasterN 1))
    , ((modm, xK_period), sendMessage (IncMasterN (-1)))
    ]
    -- FIXME: the following stuff is doing no sheet
    ++
    [((modm, k), switchNthLastFocused myTopicConfig i)
        | (i,k) <- zip [1..9] [ xK_quotedbl, xK_less, xK_greater, xK_parenleft, xK_parenright, xK_at, xK_plus, xK_minus, xK_slash ]
    ]
    
    where

        focusScreen n = screenWorkspace n >>= flip whenJust (windows . W.view)

myAdditionalKeys :: [(String, X())]
myAdditionalKeys = 
    [ ("<XF86AudioMute>"       , spawn "amixer -q set Master toggle"      ) -- toggle mute
    , ("<XF86AudioLowerVolume>", spawn "amixer -q set Master 1- unmute"    ) -- volume down 
    , ("<XF86AudioRaiseVolume>", spawn "amixer -q set Master 1+ unmute"    ) -- volume up
    , ("<XF86AudioPlay>"       , spawn "mpc toggle"     ) -- play/pause mpd
    , ("<XF86AudioStop>"       , spawn "mpc stop"       ) -- stop mpd
    , ("<XF86AudioPrev>"       , spawn "mpc previous"       ) -- prev song
    , ("<XF86AudioNext>"       , spawn "mpc next"       ) -- next song
    ] ++ scratchPadKeys myScratchPadList

--- }}}

--- ScratchPad {{{
-- | All here-defined scratchpads in a list
myScratchPadList :: [ScratchPad]
myScratchPadList = [scratchMixer, scratchTop]

--- }}}
