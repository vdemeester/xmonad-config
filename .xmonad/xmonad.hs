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
import XMonad.Actions.Promote
import XMonad.Actions.CycleWS
import XMonad.Actions.CycleRecentWS
import XMonad.Actions.CopyWindow
import XMonad.Actions.SpawnOn
import XMonad.Actions.TopicSpace as TS
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.Submap
import XMonad.Actions.Search
import XMonad.Actions.GridSelect
-- Hooks
-- Import for ToggleStruts
import XMonad.Hooks.DynamicLog as DL
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ManageHelpers (doCenterFloat)
-- For... Java...
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops
-- Layout
-- Import for smartBorder
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace   (onWorkspace, onWorkspaces)
import XMonad.Layout.SimpleFloat
import XMonad.Layout.Maximize
import XMonad.Layout.ResizableTile
import XMonad.Layout.Named
import XMonad.Layout.Grid
import XMonad.Layout.TrackFloating
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Reflect
import XMonad.Layout.ComboP
import XMonad.Layout.TwoPane
import XMonad.Layout.Tabbed
import XMonad.Layout.IM
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
import XMonad.Util.Scratchpad
import XMonad.Util.NamedWindows (getName)
-- Misc
import XMonad.StackSet (view, greedyView, tag, hidden, stack)
import qualified XMonad.StackSet as W -- to shift and float windows
import qualified Data.Map as M
-- Data
import Data.Ratio
import Data.List
import Data.Map (Map)
import Data.Monoid(mconcat)
import Data.Maybe
import Data.Ord
-- Text
-- import Text.Regex.Posix
-- System
import System.Exit
import System.Directory
import System.Environment
-- Local import
import ScratchPadKeys
import Utils
-- }}}
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

myGSConfig = defaultGSConfig
    { gs_font = themeFont }

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
-- Topic Data
data TopicType = Code
               | Other
    deriving Eq

data TopicItem = TopicItem { topicName      :: Topic
                           , topicDir       :: Dir
                           , topicAction    :: X ()
                           , topicType      :: TopicType
                           }

-- Topic definition, lots of !
myDefaultTopicAction = return()

myDefaultTopicConfig = TopicConfig
    { topicDirs             = M.empty
    , topicActions          = M.empty
    , defaultTopicAction    = const $ myDefaultTopicAction
    , defaultTopic          = "admin"
    , maxTopicHistory       = 10
    }

-- actions
myActionTopics' :: [(Topic, X())]
myActionTopics' = [ ("admin",   spawnScreenSession "default" >> spawnT (myTerminal ++ " -e 'tmx default'")) -- FIXME
                  , ("music",   spawnT (myTerminal ++ " -e ncmpcpp"))
                  , ("chat",    spawn "pidgin")
                  , ("gimp",    spawn "gimp")]

myActionTopics :: [(Topic, Dir, X ())]
myActionTopics = map (\(n, a) -> (n, "", a)) myActionTopics'
               ++ [ ("conf", "etc", codeTopicAction)
                 ]

myCodeTopics = [ ("xmonad", ".xmonad")
               , ("chef", "src/work/chef/")
               , ("debian", "src/debian")
               ]

myOtherTopics = [ "com"
                , "web"
                , "documents"
                ]

myTopics :: [TopicItem]
myTopics = map topicItem'' myActionTopics
           ++
           map codeTopicItem myCodeTopics
           ++
           map otherTopicItem myOtherTopics
    where
        topicItem'' (name, dir, action) = topicItem' name dir action
        codeTopicItem (name, dir) = topicItem name dir (codeTopicSession' myTopicConfig name) Code
        otherTopicItem name = topicItem' name "" myDefaultTopicAction

topicDirMap :: [TopicItem] -> Map Topic Dir
topicDirMap ts = M.fromList $ map (\(TopicItem n d _ _) -> (n, d)) ts

topicActionMap :: [TopicItem] -> Map Topic (X ())
topicActionMap ts = M.fromList $ map (\(TopicItem n _ a _) -> (n, a)) ts

updateTopicConfig :: TopicConfig -> [TopicItem] -> TopicConfig
updateTopicConfig tc ts = tc
    { topicDirs = topicDirs tc <+> (topicDirMap ts)
    , topicActions = topicActions tc <+> (topicActionMap ts)
    }

myTopicConfig = updateTopicConfig myDefaultTopicConfig myTopics


topicItem :: Topic -> Dir -> X () -> TopicType -> TopicItem
topicItem name dir action ttype = TopicItem name topicdir action ttype
    where topicdir =
              case dir of
                   "" -> "${HOME}/"
                   _ -> dir

topicItem' :: Topic -> Dir -> X () -> TopicItem
topicItem' name dir action = topicItem name dir action Other

typedTopicItem :: TopicConfig -> Topic -> Dir -> String -> TopicItem
typedTopicItem tc name dir ttype | ttype == "code" = topicItem name dir (codeTopicSession' tc name) Code
                                 | otherwise = topicItem' name dir myDefaultTopicAction

-- external topic file
-- format: multiple lines with name, dir and type each
-- topic1 topicdir1 code
-- topic2 topicdir2
-- topic3 topicdir3 code
-- ...
myTopicFile = ".xmonad/topics"
myCodeTopicFile = ".xmonad/code-topics"

zipTopics' :: TopicConfig -> String -> String -> [TopicItem]
zipTopics' tc dc s = (map myZip) ( (map words) (lines s) )
    where
        myZip (x:y:z:xs) = typedTopicItem tc x y z
        myZip (x:y:xs) = typedTopicItem tc x y dc
zipTopics tc s = zipTopics' tc "other" s

readTopicsFile :: String -> IO String
readTopicsFile f = do
    e <- doesFileExist f
    if e then readTopicsFile' f
         else return ""

readTopicsFile' :: String -> IO String
readTopicsFile' f = do
    l <- readFile f
    return $ l

myTopicNames :: [Topic]
myTopicNames = map topicName myTopics

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

spawnShell :: TopicConfig -> X ()
spawnShell tc = currentTopicDir tc >>= spawnShellIn

spawnT' :: TopicConfig -> String -> X()
spawnT' tc program = currentTopicDir tc >>= spawnIn program
spawnT program = spawnT' myTopicConfig program

spawnIn :: String -> Dir -> X ()
spawnIn program dir = spawn $ "cd ''" ++ dir ++ "'' && " ++ program ++ " &"
{-spawnIn program dir = spawn $ myTerminal ++ "'(cd " ++ dir ++ " && zsh )'"-}

spawnShellIn :: Dir -> X ()
spawnShellIn dir = spawnIn myTerminal dir

spawnScreenSession' :: TopicConfig -> String -> X ()
spawnScreenSession' tc session = currentTopicDir tc >>= spawnScreenSessionIn session

spawnScreenSession :: String -> X ()
spawnScreenSession session = spawnScreenSession' myTopicConfig session

screenSession session = myTerminal ++ " -e tmx " ++ session
spawnScreenSessionIn :: String -> Dir -> X ()
spawnScreenSessionIn session dir = spawnIn (screenSession session) dir

gvimSession tc session = spawnT' tc (myTerminal ++ " -e vim -c ':SessionOpen " ++ session ++ "' -c 'let v:this_session = \"" ++ session ++ "\"'")

-- code topics
codeTopicAction' tc = spawnShell tc >> spawnT' tc (myTerminal ++ " -e vim")
codeTopicAction = codeTopicAction' myTopicConfig

codeTopicSession :: TopicConfig -> String -> (String, X () )
codeTopicSession tc topic = (topic, codeTopicSession' tc topic)

codeTopicSession' :: TopicConfig -> String -> X ()
codeTopicSession' tc topic = spawnScreenSession' tc topic >> gvimSession tc topic

--- }}}
--- ManageHooks {{{
myManageHook :: ManageHook
myManageHook = composeAll [ matchAny v --> a | (v,a) <- myActions ]  <+> manageScratchPads scratchPadList

    where myActions = [ ("rdesktop"  , doFloat         )
                      , ("Xmessage"  , doCenterFloat   )
                      , ("Iceweasel" , doShift "web" )
                      , ("Firefox"   , doShift "web" )
                      , ("irssi"     , doShift "irc")
                      ]
-- UrgencyHook
-- We are going to use notify-send
data NotifyUrgencyHook = NotifyUrgencyHook deriving (Read, Show)

instance UrgencyHook NotifyUrgencyHook where
    urgencyHook NotifyUrgencyHook w = do
        name <- getName w
        ws <- gets windowset
        whenJust (W.findTag w ws) (flash name)
      where flash name index =
                  spawn $ "notify-send " ++ "\"Urgent Window\" \"<b>" ++ (show name ++ "</b> requests your attention on workspace <b>" ++ index) ++ "</b>\""
--- }}}
--- Layout {{{
tiledModifiers a = mkToggle1 NBFULL
                 $ maximize
                 $ a

tiled ratio = named "tall" $ tiledModifiers $ ResizableTall nmaster delta ratio []
    where
        nmaster = 1
        delta = 3/100

halfTiled = tiled $ 1/2
tiledMirror = named "mirror" $ Mirror $ tiled $ 1/2
codeMirror = named "code" $ Mirror $ tiled $ 4/5

layoutCode = codeMirror ||| halfTiled ||| Full
layoutGimp = named "gimp"
           $ combineTwoP (TwoPane 0.85 0.15) Full
             (combineTwoP (reflectHoriz $ TwoPane 0.25 0.25)
              simpleTabbed
              (simpleTabbed ||| Full ||| halfTiled)
              (Role "gimp-dock")
             )
             (Role "gimp-toolbox")

layoutPidgin = named "IM"
             $ reflectHoriz
             $ withIM size roster
-- $ reflectHoriz
             $ layout
    where
        layout = Grid
        size = 1%5
        roster = Title "Buddy List"

myLayoutHook ts = avoidStruts
                $ onWorkspace "chat"  layoutPidgin
                $ onWorkspace "web"   webLayout
                $ onWorkspace "gimp"  layoutGimp
                $ onWorkspaces codeWS layoutCode
                $ standardLayouts
    where
        -- specific layouts
        -- imLayout = withIM (2/10) pidginRoster Grid
        webLayout = smartBorders $ full ||| (tabbed shrinkText myTheme) ||| Grid
        codeWS = [ topicName t | t <- ts, topicType t == Code ]
        -- standard layouts
        standardLayouts = smartBorders $ Mirror tiled ||| full ||| tiled ||| misc
        tiled = Tall 1 (2/100) (4/5)
        full  = Full
        misc  = Grid ||| tabbed shrinkText myTheme
        -- pidginRoster    = And (ClassName "Pidgin") (Role "buddy_list")
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
myKeys tc conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm .|. controlMask, xK_Return), spawnHere $ XMonad.terminal conf)
    , ((modm .|. shiftMask, xK_Return), spawnShell tc) -- start shell in topic dir
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
    , ((modm .|. shiftMask, xK_m), withFocused (sendMessage . maximizeRestore))
    , ((modm .|. shiftMask, xK_t), windows W.swapDown)
    , ((modm .|. shiftMask, xK_s), windows W.swapUp)
    , ((modm, xK_c), sendMessage Shrink)
    , ((modm, xK_r), sendMessage Expand)
    , ((modm .|. controlMask, xK_space), sendMessage $ Toggle NBFULL)
    , ((modm, xK_b), sendMessage ToggleStruts)
    -- Prompt(s)
    , ((modm, xK_F1), manPrompt myXPConfig)
    , ((modm, xK_j), shellPromptHere myXPConfig)
    , ((modm .|. shiftMask, xK_j), runOrRaisePrompt myXPConfig)
    , ((modm .|. controlMask, xK_s), sshPrompt myXPConfig)
    -- Prompt(s) workspace/topic
    , ((modm, xK_f), gridselectTopic tc myGSConfig)
    , ((modm, xK_o), workspacePrompt myXPConfig (addTopic tc))
    , ((modm .|. shiftMask, xK_BackSpace), removeWorkspace)
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
--- Utils {{{
myToggleWS :: X ()
myToggleWS = windows $ view =<< tag . head . scratchpadFilterOutWorkspace . hidden

myCycleRecentWS = myRecentWS W.view
myShiftRecentWS = myRecentWS shiftView'
    where shiftView' id ws = W.view id $ W.shift id ws

myRecentWS f = cycleWindowSets options
    where
        options w = map (f `flip` w) (recentTags w)
        recentTags w = map tag $ tail (myWS w) ++ [head (myWS w)]
        myWS w = scratchpadFilterOutWorkspace $ W.workspaces w

addTopic :: TopicConfig -> String -> X ()
addTopic tc newtag = addHiddenTopic newtag >> switchTopic tc newtag

addHiddenTopic :: String -> X ()
addHiddenTopic newtag = addHiddenWorkspace newtag

gridselectTopic :: TopicConfig -> GSConfig WorkspaceId -> X ()
gridselectTopic tc conf = withWindowSet $ \ws -> do
    let wss = map W.tag $ W.hidden ws ++ map W.workspace (W.current ws : W.visible ws)
    gridselect conf (zip wss wss) >>= flip whenJust (switchTopic tc)

mergePPOutputs :: [PP -> X String] -> PP -> X String
mergePPOutputs x pp = fmap (intercalate (ppSep pp)) . sequence . sequence x $ pp

onlyTitle :: PP -> PP
onlyTitle pp = defaultPP { ppCurrent = const ""
                         , ppHidden = const ""
                         , ppVisible = const ""
                         , ppLayout = ppLayout pp
                         , ppTitle = ppTitle pp }
-- taken from XMonad.Actions.TopicSpace and modified for respecting ppSort
-- function
mypprWindowSet :: TopicConfig -> PP -> X String
mypprWindowSet tg pp = do
    winset <- gets windowset
    urgents <- readUrgents
    sort' <- ppSort pp
    let empty_workspaces = map W.tag $ filter (isNothing . W.stack) $ W.workspaces winset
        maxDepth = maxTopicHistory tg
    setLastFocusedTopic (W.tag . W.workspace . W.current $ winset)
                        (`notElem` empty_workspaces)
    lastWs <- getLastFocusedTopics
    let depth topic = fromJust $ elemIndex topic (lastWs ++ [topic])
        add_depth proj topic = proj pp . (((topic++":")++) . show) . depth $ topic
        pp' = pp { ppHidden = add_depth ppHidden, ppVisible = add_depth ppVisible }
        sortWindows = take maxDepth . sortBy (comparing $ depth . W.tag) . sort'
    return $ DL.pprWindowSet sortWindows urgents pp' winset


myDynamicLogString :: TopicConfig -> PP -> X String
myDynamicLogString tc pp = mergePPOutputs [mypprWindowSet tc, dynamicLogString . onlyTitle] pp

myDynamicLogWithPP :: TopicConfig -> PP -> X ()
myDynamicLogWithPP tc pp = myDynamicLogString tc pp >>= io . ppOutput pp
--- }}}
--- Config {{{
myConfig = withUrgencyHookC NotifyUrgencyHook urgencyConfig { suppressWhen = Focused }
         $ defaultConfig
         { borderWidth = 1
         , modMask               = mod4Mask -- use the Windows button as mod
         , terminal = myTerminal
         -- , workspaces            = myTopics
         -- , layoutHook            = myLayout
         , normalBorderColor     = "#191919"
         -- , normalBorderColor = "#333333"
         -- , focusedBorderColor    = "#131313"
         , focusedBorderColor = "#afdf87"
         -- , startupHook           = startupHook defaultConfig >> setWMName "LG3D"
         , startupHook = ewmhDesktopsStartup <+> setWMName "LG3D"
         , handleEventHook = ewmhDesktopsEventHook
         , manageHook            = manageHook defaultConfig <+> manageSpawn <+> myManageHook <+> manageScratchPads myScratchPadList
         -- , manageHook = manageSpawn <+> myManageHook
         , focusFollowsMouse     = False
         }

updateMyConfig conf home tc ts = conf
    { workspaces = map topicName ts
    , layoutHook = myLayoutHook ts
    }
--- }}}
--- Main {{{
main = do
    home <- getEnv "HOME"
    tf   <- readTopicsFile $ home ++ "/" ++ myTopicFile
    ctf  <- readTopicsFile $ home ++ "/" ++ myCodeTopicFile
    let ts   = zipTopics myTopicConfig tf
             ++ zipTopics' myTopicConfig "code" ctf
    let tc   = updateTopicConfig myTopicConfig ts
    let conf = updateMyConfig myConfig home tc $ myTopics ++ ts
    -- checkTopicConfig myTopics myTopicConfig
    xmproc <- spawnPipe "xmobar"
    xmonad $ conf
        -- { logHook = logHook conf
        { logHook               = dynamicLogWithPP $ myPP { ppOutput = hPutStrLn xmproc }
        , keys                  = myKeys tc
        } `additionalKeysP` myAdditionalKeys
--- }}}

-- vim:foldmethod=marker foldmarker={{{,}}}
