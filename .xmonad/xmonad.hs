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
-- Up to date xmonad configuration, using xmonad-0.10.0 packaged on Debian 
-- testing (wheezy)
--
-------------------------------------------------------------------------------
{-# LANGUAGE
     DeriveDataTypeable
     #-}
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
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ManageHelpers (doCenterFloat)
-- Layout
-- Import for smartBorder
import XMonad.Layout.NoBorders
import XMonad.Layout.IM             (Property(..), withIM)
import XMonad.Layout.PerWorkspace   (onWorkspace)
-- Tests
import XMonad.Layout.Grid
import XMonad.Actions.GridSelect
import XMonad.Layout.Tabbed
-- Prompt(s)
import XMonad.Prompt
import XMonad.Prompt.Man
import XMonad.Prompt.RunOrRaise
import XMonad.Prompt.Shell
import XMonad.Prompt.Ssh
import XMonad.Prompt.AppendFile
import XMonad.Prompt.Workspace
-- Topics
import XMonad.Actions.TopicSpace
-- Util
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Run
-- Misc
import qualified XMonad.StackSet as W -- to shift and float windows
import qualified Data.Map as M
import Data.Monoid(mconcat)
import Data.List
-- System
import System.Exit
import System.Directory
import System.Environment
-- Local import
import ScratchPadKeys
import Utils
-- }}}

--- Main {{{
main = do
    home <- getEnv "HOME"
    checkTopicConfig myTopics myTopicConfig
    xmproc <- spawnPipe "xmobar"
    xmonad $ withUrgencyHookC NoUrgencyHook urgencyConfig { suppressWhen = Focused } $ defaultConfig
        { terminal              = myTerminal
        , workspaces            = myTopics
        , modMask               = mod4Mask -- use the Windows button as mod
        , manageHook            = manageHook defaultConfig <+> manageSpawn <+> myManageHook <+> manageScratchPads myScratchPadList
        , layoutHook            = myLayout
        , logHook               = dynamicLogWithPP $ myPP { ppOutput = hPutStrLn xmproc }
        , keys                  = myKeys
        , focusFollowsMouse     = False
        , focusedBorderColor    = "#131313"
        , normalBorderColor     = "#191919"
        } `additionalKeysP` myAdditionalKeys
--- }}}

--- Options {{{
-- Misc.
myTerminal          = "urxvtc"
myShell             = "$SHELL"
myBorderWidth       = 1
-- Fonts (xft :-) & co)
barFont             = "Play-8"
-- themeFont           = "xft:Play:size=8"
-- themeFont           = "xft:DejaVu Sans Mono:size=8:antialias=true"
themeFont           = "xft:Droid Sans Mono:size=8:antialias=true"
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
green               = "#afdf87"
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
    -- Auto complete and "hit return" when only choice
    -- , autoComplete = Just 400000
    }

myNoteXPConfig = myXPConfig
    { position  = Bottom
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
    , ppTitle   = xmobarColor "#dfafdf" "" . shorten 60
    , ppCurrent = xmobarColor "#1c1c1c" "#dfaf87" . pad
    , ppSep     = xmobarColor "#1C1C1C" "" " "
    , ppUrgent  = xmobarColor "#1C1C1C" "#df8787" . wrap " " " " . xmobarStrip
    , ppLayout  = xmobarColor "#1C1C1C" "#444444" . pad . \s ->
        case s of
            "Tall"                  -> "▥"
            "Mirror Tall"           -> "▤"
            "Full"                  -> "□"
            "Grid"                  -> "▦"
            "IM Grid"               -> "▩"
            "Tabbed Simplest"       -> "▔"
            _                       -> pad s
    }
--- }}}

--- Topics (Workspaces) {{{
-- Topic definition, lots of !
myTopics :: [Topic]
myTopics =
    [ "default" -- the default one
    , "web" -- firefox, thunderbird
    , "dev", "eclipse", "sites" -- dev
    , "music", "video", "pictures" -- multimedia
    , "chat" -- chat
    , "doc", "ebook" -- documents
    , "games" -- games
    , "test", "wip"
    ]
-- Topic configuration
myTopicConfig :: TopicConfig
myTopicConfig = TopicConfig
    { topicDirs = M.fromList $
        [ ("sites", "src/sites")
        , ("music", "music")
        , ("video", "video")
        , ("pictures", "pictures")
        , ("doc", "documents")
        , ("ebook", "documents/ebook")
        , ("games", "games")
        ]
    , defaultTopicAction = const $ spawnShell
    , defaultTopic = "default"
    , topicActions = M.fromList $
        [ ("music", spawn $ myTerminal ++ " -e ncmpcpp")
        , ("chat", spawn "pidgin")
        , ("eclipse", spawn "eclipse")
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
spawnShellIn dir = spawn $ myTerminal ++ " -title urxvt -e sh -c 'cd ''" ++ dir ++ "'' && " ++ myShell ++ " -l'"
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
myLayout = avoidStruts $ onWorkspace "chat" imLayout $ onWorkspace "web" webLayout $ onWorkspace "eclipse" eclipseLayout $ standardLayouts
    where
        -- specific layouts
        imLayout = withIM (2/10) pidginRoster Grid
        webLayout = smartBorders $ full ||| (tabbed shrinkText myTheme) ||| Grid
        eclipseLayout = smartBorders $ full ||| Grid
        -- standard layouts
        standardLayouts = smartBorders $ Mirror tiled ||| full ||| tiled ||| misc
        tiled = Tall 1 (2/100) (4/5)
        full  = Full
        misc  = Grid ||| tabbed shrinkText myTheme
        pidginRoster    = And (ClassName "Pidgin") (Role "buddy_list")
        skypeRoster     = (ClassName "Skype") `And` (Not (Title "Options")) `And` (Not (Role "Chats")) `And` (Not (Role "CallWindowForm"))
        psiRoster       = (And (ClassName "psi") (Resource "main"))
--- }}}

--- MPD {{{
-- Prompt & stuff for mpd
newtype HostPrompt = HostPrompt { hostPrompt :: String } deriving (Read,Show,Typeable)
instance ExtensionClass HostPrompt where
    initialValue = HostPrompt "127.0.0.1"
    extensionType = PersistentExtension

instance XPrompt HostPrompt where showXPrompt _ = "Pick MPD Host: "
promptHost = mkXPrompt (HostPrompt "") myXPConfig (return . compl) (XS.put . HostPrompt)
    where compl s = nub $ filter (s `isPrefixOf`) ["127.0.0.1","192.168.1.11","172.20.1.199"]
-- FIXME Use hostname !
mpcAct c = do
    h <- XS.gets hostPrompt
    spawn $ unwords ["export MPD_HOST="++h,";","mpc",c]
-- }}}

--- Keys {{{
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm .|. controlMask, xK_Return), spawnHere $ XMonad.terminal conf)
    , ((modm .|. shiftMask, xK_Return), spawnShell) -- start shell in topic dir
    , ((modm, xK_d), spawnDolphin) -- start shell in topic dir
    --[ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
    , ((modm, xK_space), sendMessage NextLayout)
    , ((modm .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)
    , ((modm, xK_Return), windows W.swapMaster)
    , ((modm .|. controlMask, xK_n), refresh)
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
    , ((modm, xK_j), shellPromptHere myXPConfig)
    , ((modm .|. shiftMask, xK_j), runOrRaisePrompt myXPConfig)
    , ((modm .|. controlMask, xK_s), sshPrompt myXPConfig)
    -- Prompt(s) search
    , ((modm, xK_F2), submap $ searchEngineMap $ promptSearch myXPConfig)
    , ((modm .|. shiftMask, xK_F2), submap $ searchEngineMap $ selectSearch)
    -- Prompt note taking
    , ((modm .|. shiftMask, xK_n),               appendFilePrompt myNoteXPConfig ("documents/.notes"))
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
    -- Urgent !!!
    , ((modm, xK_y), focusUrgent)
    -- Restarting
    , ((modm .|. controlMask .|. shiftMask, xK_q),           io (exitWith ExitSuccess))
    , ((modm, xK_q), cleanStart)
    -- Push window back into tiling
    , ((modm, xK_l), withFocused $ windows . W.sink)
    , ((modm, xK_comma), sendMessage (IncMasterN 1))
    , ((modm, xK_period), sendMessage (IncMasterN (-1)))
    -- Tests
    , ((modm .|. controlMask, xK_g), goToSelected defaultGSConfig)
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
    , ("S-<XF86AudioPlay>"       , promptHost     ) -- play/pause mpd
    , ("<XF86AudioPlay>"       , do mpcAct "toggle"     ) -- play/pause mpd
    , ("<XF86AudioStop>"       , do mpcAct "stop"       ) -- stop mpd
    , ("<XF86AudioPrev>"       , do mpcAct "previous"       ) -- prev song
    , ("<XF86AudioNext>"       , do mpcAct "next"       ) -- next song
--    , ("<XF86AudioPlay>"       , spawn "mpc toggle"     ) -- play/pause mpd
--    , ("<XF86AudioStop>"       , spawn "mpc stop"       ) -- stop mpd
--    , ("<XF86AudioPrev>"       , spawn "mpc previous"       ) -- prev song
--    , ("<XF86AudioNext>"       , spawn "mpc next"       ) -- next song
    ] ++ scratchPadKeys myScratchPadList

--- }}}

--- ScratchPad {{{
-- | All here-defined scratchpads in a list
myScratchPadList :: [ScratchPad]
myScratchPadList = [scratchMixer, scratchTop, scratchTerminal, scratchMutt, scratchMusic, scratchVim]

--- }}}
-- vim:foldmethod=marker foldmarker={{{,}}}
