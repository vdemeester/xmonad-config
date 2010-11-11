-------------------------------------------------------------------------------
-- |
-- Module      :  xmonad.hs
-- Copyright   :  (c) Patrick Brisbin 2010 
-- License     :  as-is
--
-- Maintainer  :  pbrisbin@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- Up to date [testing], using -darcs, last modified 16 September 2010.
--
-- vim:foldmethod=marker foldmarker={{{,}}}
-------------------------------------------------------------------------------

-- Imports {{{

-- my lib
import Dzen           -- http://pbrisbin.com/xmonad/docs/Dzen.html
import ScratchPadKeys -- http://pbrisbin.com/xmonad/docs/ScratchPadKeys.html
import SendFile       -- http://pbrisbin.com/xmonad/docs/SendFile.html
import RssReader      -- http://pbrisbin.com/xmonad/docs/RssReader.html

-- xmonad
import XMonad hiding ((|||))
import qualified XMonad.StackSet as W

-- xmonad-contrib
import XMonad.Actions.CycleWS            (toggleWS)
import XMonad.Actions.FindEmptyWorkspace (tagToEmptyWorkspace, viewEmptyWorkspace)
import XMonad.Actions.Warp               (Corner(..), banishScreen)
import XMonad.Actions.WithAll            (killAll)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops         (ewmh)
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.IM                  (Property(..), withIM)
import XMonad.Layout.LayoutCombinators   ((|||), JumpToLayout(..))
import XMonad.Layout.LayoutHints         (layoutHintsWithPlacement)
import XMonad.Layout.NoBorders           (Ambiguity(..), With(..), lessBorders)
import XMonad.Layout.PerWorkspace        (onWorkspace)
import XMonad.Layout.Magnifier
import XMonad.Layout.ResizableTile       (ResizableTall(..), MirrorResize(..))
import XMonad.Layout.Accordion
import XMonad.Layout.Grid
import XMonad.Layout.LimitWindows
import XMonad.Layout.BoringWindows
import XMonad.Util.EZConfig              (additionalKeysP)
import XMonad.Util.Loggers               (Logger, maildirNew, dzenColorL, wrapL, shortenL)
import XMonad.Util.Run                   (spawnPipe)
import XMonad.Util.WindowProperties      (getProp32s)
import XMonad.Util.WorkspaceCompare      (getSortByXineramaRule)

import qualified XMonad.Prompt as P
import qualified XMonad.StackSet as W

-- general haskell stuff
import Data.List      (isPrefixOf)
import System.IO      (Handle, hPutStrLn, hGetContents)
import System.Process (runInteractiveCommand)
import qualified Data.Map as M
import Graphics.X11.Xlib
-- }}}

-- Main {{{
main :: IO ()
main = do
    d <- spawnDzen myLeftBar
    -- d <- spawnPipe . show myLeftBar ++ "-w $(echo $((`~/.bin/getResolution.sh` - " ++ show rightBarWidth ++ ")))"

    --spawn "conky"
    -- spawn $ "conky -c ~/.dzen_conkyrc | " ++ show myRightBar
    spawn $ "conky -c ~/.dzen_conkyrc | " ++ show myRightBar ++ " -x  $(echo $((`~/.bin/getResolution.sh` - " ++ rightBarWidth ++ ")))"
    -- spawnDzen myRssBar >>= spawnReader myReaderConf

    -- ewmh just makes wmctrl work
    xmonad $ ewmh $ withUrgencyHookC myUrgencyHook myUrgencyConfig $ defaultConfig
        { terminal           = myTerminal
        , workspaces         = myWorkspaces
		, keys				 = myKeys
        , borderWidth        = myBorderWidth
        , normalBorderColor  = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
		, focusFollowsMouse	 = myFocusFollowsMouse
        , layoutHook         = myLayout
        , manageHook         = myManageHook
        , logHook            = myLogHook d
        } `additionalKeysP` myAdditionalKeys

-- }}}

-- Options {{{àa
myTerminal           = "urxvtc"
myBorderWidth        = 1
myNormalBorderColor  = "#111111"
myFocusedBorderColor = "#D75F00"
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

-- if you change workspace names, be sure to update them throughout
myWorkspaces = ["1-main","2-web","3-chat","4-dev"] ++ map show [5..9]
--myWorkspaces = [ supWsNum "1" "dev"
--  , supWsNum "2" "web"
--  , supWsNum "3" "chat"
--  , supWsNum "4" "mail"
--  , supWsNum "5" ""
--  , supWsNum "6" ""
--  , supWsNum "7" ""
--  , supWsNum "8" ""
--  , supWsNum "9" ""
--  ]
--  where
--    supWsNum wsName wsNum = if any (`elem` wsNum) ['a'..'z'] then wsName ++  "^p(;_TOP)^fn(" ++ mySmallFont  ++ ")" ++ wsNum ++ "^fn()^p()" else wsName
-- if any (`elem` ws) ['a'..'z'] then pad ws else ""


-- aur/dzen2-svn is required for an xft font
--myFont = "Envy Code R-8"
myFont = "Droid Sans Mono-8"
mySmallFont = "xft:Droid Sans Mono:size=5"

-- background/foreground and various levels of emphasis
colorBG  = "#606060"
colorFG  = "#C0C0C0"
colorFG2 = "#E0E0E0"
colorFG3 = "#c4df90"
colorFG4 = "#cc896d"
colorFG5 = "#c4df90"
colorFG6 = "#ffffba"

-- status bar sizes
leftBarWidth  = "$(echo $((`~/.bin/getResolution.sh` - " ++ rightBarWidth ++ ")))"
rssBarWidth   = "512"
rightBarWidth = "424"

-- }}}

-- Layouts {{{
--
-- See http://pbrisbin.com/pages/im-layout.html#update
--
myLayout = avoidStruts $ onWorkspace "3-chat" imLayout $ onWorkspace "4-dev" devLayout  $ standardLayouts

    where
        -- use standardLayouts just like any other workspace
        imLayout = withIM (3/10) (Role "roster") Grid

        devLayout = limitWindows 4 $ MAgnifiercz' 1.4 $ smart $ devTiled ||| Mirror devTiled ||| full

        -- a simple Tall, Wide, Full setup but hinted, resizable, and
        -- with smarter borders
        standardLayouts = smart $ tiled ||| Mirror tiled |||  full

        tiled = Tall 1 (2/100) Golden
		devTiled = Tail 1 (2/100) 0.75
        full  = Full

        -- master:slave set at the golden ratio
        golden = toRational $ 2/(1 + sqrt 5 :: Double)

        -- just like smartBorders but better for a xinerama setup
        smart = lessBorders $ Combine Union OnlyFloat OtherIndicated

        -- like hintedTile but appliable to any layout
        hinted l = layoutHintsWithPlacement (0,0) l

-- }}}

-- ManageHook {{{
myManageHook :: ManageHook
myManageHook = mainManageHook <+> manageDocks <+> manageFullScreen <+> manageScratchPads scratchPadList

    where
        -- the main managehook
        mainManageHook = composeAll $ concat
            [ [ resource  =? r     --> doIgnore         |  r    <- myIgnores ]
            , [ className =? c     --> doShift "2-web"  |  c    <- myWebs    ]
            , [ title     =? t     --> doShift "3-chat" |  t    <- myChats   ]
            , [ className =? c     --> doShift "3-chat" | (c,_) <- myIMs     ]
            , [ className =? c     --> doFloat          |  c    <- myFloats  ]
            , [ className =? c     --> doCenterFloat    |  c    <- myCFloats ]
            , [ name      =? n     --> doCenterFloat    |  n    <- myCNames  ]
            , [ classNotRole (c,r) --> doFloat          | (c,r) <- myIMs     ]
            , [ isDialog           --> doCenterFloat                         ]
            ]

        -- fullscreen but still allow focusing of other WSs
        manageFullScreen = isFullscreen --> doF W.focusDown <+> doFullFloat

        -- a special query to find an im window that's not my buddy list
        classNotRole :: (String,String) -> Query Bool
        classNotRole (c,r) = className =? c <&&> role /=? r

        role = stringProperty "WM_WINDOW_ROLE"
        name = stringProperty "WM_NAME"

        myIMs     = [("Gajim.py","roster")]
        myIgnores = ["desktop","desktop_window"]
        myChats   = ["irssi","mutt" ]
        myWebs    = ["Uzbl","Uzbl-core","Jumanji","Firefox"]
        myFloats  = ["MPlayer","Zenity","VirtualBox","rdesktop"]
        myCFloats = ["Xmessage","Save As...","XFontSel"]
        myCNames  = ["bashrun"]

-- }}}

-- StatusBars {{{
--
-- See http://pbrisbin.com/xmonad/docs/Dzen.html
--
myLeftBar :: DzenConf
myLeftBar = defaultDzen
    { width       = leftBarWidth
    , font        = myFont
--    { font        = myFont
    , fg_color    = colorFG
    , bg_color    = colorBG
    }

myRssBar :: DzenConf
myRssBar = myLeftBar
    { x_position = leftBarWidth
    , width      = rssBarWidth
    }

myRightBar :: DzenConf
myRightBar = myLeftBar
    { x_position = leftBarWidth
	, width = rightBarWidth
    , alignment  = RightAlign
--	, exec  = ""
    }

-- }}}

-- RssReader {{{
--
-- See http://pbrisbin.com/xmonad/docs/RssReader.html
--
myReaderConf :: ReaderConf
myReaderConf = defaultReaderConf
    -- stealing some dynamicLog functions here
    { titleFormat = wrap "" ":" . dzenColor "#909090" "" . dzenEscape
    , descrFormat = dzenEscape . shorten 200
    }

-- }}}

-- Prompt {{{
promptConfig :: P.XPConfig
promptConfig = P.defaultXPConfig 
    { P.font        = myFont
    , P.bgColor     = colorBG
    , P.fgColor     = colorFG2
    , P.fgHLight    = colorFG4
    , P.bgHLight    = colorBG
    , P.borderColor = colorFG
    }

-- }}}

-- LogHook {{{
myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ defaultPP
        { ppCurrent         = dzenFG colorFG5 . pad
        , ppVisible         = dzenFG colorFG2 . pad
        , ppHidden          = dzenFG colorFG2 . noScratchPad
        , ppHiddenNoWindows = namedOnly
        , ppUrgent          = dzenFG colorFG4 . pad . dzenStrip
        , ppSep             = replicate 1 ' '
        , ppWsSep           = []
        , ppTitle           = shorten 100 
        , ppLayout          = dzenFG colorFG2 . renameLayouts . stripIM
        , ppSort            = getSortByXineramaRule
        , ppExtras          = [myMail, myUpdates]
        , ppOutput          = hPutStrLn h
        }

    where
		-- don't show 4-9 if they're empty, never show NSP
        namedOnly    ws = if any (`elem` ws) ['a'..'z'] then pad ws else ""
        noScratchPad ws = if ws /= "NSP"                then pad ws else ""

        -- L needed for loggers
        dzenFG  c = dzenColor  c ""
        dzenFGL c = dzenColorL c "" 

        -- custom loggers
        myMail    = wrapL "Mail: "    "" . dzenFGL colorFG6 $ maildirNew "/home/vincent/.mail/personal/INBOX"
        myUpdates = wrapL "Updates: " "" . dzenFGL colorFG6 $ countOutputLines "pacman -Qu"
        
        -- count the lines of output of an arbitary command
        countOutputLines :: String -> Logger
        countOutputLines c = io $ do
            (_, out, _, _) <- runInteractiveCommand c
            (doCount out) `catch` (const $ return Nothing)
        
            where
                -- 0 returns nothing
                doCount h = hGetContents h >>= \c ->
                    case length $ lines c of
                        0 -> return Nothing
                        n -> return $ Just $ show n

        renameLayouts s = case s of
            "Tall"                             -> "[-] "
            "Magnifier NoMaster Tall"          -> "[-]°"
            "Mirror Tall"                      -> "[±] "
            "Magnifier NoMaster Mirror Tall"   -> "[±]°"
            "Accordion"                        -> "[=] "
            "Magnifier NoMaster Accordion"     -> "[=]°"
            "Grid"                             -> "[+] "
            "Magnifier NoMaster Grid"          -> "[+]°"
            "Full"                             -> "[ ] "
            "Magnifier NoMaster Full"          -> "[ ]°"
            _                                  -> s

        stripIM s = if "IM " `isPrefixOf` s then drop (length "IM ") s else s

-- }}}

-- SpawnHook {{{
--
-- Spawn any arbitrary command on urgent
--
data MySpawnHook = MySpawnHook String deriving (Read, Show)

instance UrgencyHook MySpawnHook where
    urgencyHook (MySpawnHook s) w = spawn s

myUrgencyHook :: MySpawnHook
myUrgencyHook = MySpawnHook "aplay -q /usr/share/gajim/data/sounds/message2.wav" 

myUrgencyConfig :: UrgencyConfig
myUrgencyConfig = UrgencyConfig OnScreen (Repeatedly 1 30)

-- }}}

-- KeyBindings {{{
-- Default ?
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
	-- Push window back into tiling
	, ((modm, xK_l), withFocused $ windows . W.sink)
	, ((modm, xK_comma), sendMessage (IncMasterN 1))
	, ((modm, xK_period), sendMessage (IncMasterN (-1)))
	]
	++
	[((m .|. modm, k), windows $ f i)
		| (i,k) <- zip (XMonad.workspaces conf) [ xK_quotedbl, xK_guillemotleft, xK_guillemotright, xK_parenleft, xK_parenright, xK_at, xK_plus, xK_minus, xK_slash ]
		, (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
-- Additional(s)
myAdditionalKeys :: [(String, X())]
myAdditionalKeys = [ ("M-j"                   , spawn "~/.bin/launcher"       ) -- dmenu app launcher
         , ("M-S-j"                 , spawn "bashrun"        ) -- gmrun replacement
         , ("M4-f"                  , sendFile               ) -- prompt for and send a file via mutt
         , ("M4-m"                  , myMail                 ) -- open mail client
         , ("M4-b"                  , myBrowser              ) -- open web client
         , ("M4-e"                  , myEject                ) -- open/close tray 
         , ("M4-l"                  , myLock                 ) -- W-l to lock screen
         , ("M4-i"                  , myIRC                  ) -- open/attach IRC client in screen
         , ("M4-r"                  , myTorrents             ) -- open/attach rtorrent in screen 
         , ("M-a"                   , spawn "msearch all"    ) -- search current playlist via dmenu
         , ("M-g"                   , spawn "goodsong"       ) -- note current song as 'good'
         , ("M-S-g"                 , spawn "goodsong -p"    ) -- play a random 'good' song
         , ("<Print>"               , spawn "sshot"          ) -- take a screenshot

         -- extended workspace navigations
         , ("M-$"                   , toggleWS               ) -- switch to the most recently viewed ws
         , ("M-<Backspace>"         , focusUrgent            ) -- focus most recently urgent window
         , ("M-S-<Backspace>"       , clearUrgents           ) -- make urgents go away
         , ("M-0"                   , viewEmptyWorkspace     ) -- go to next empty workspace
         , ("M-S-0"                 , tagToEmptyWorkspace    ) -- send window to empty workspace and view it

         -- extended window movements
         , ("M-o"                   , mirrorShrink           ) -- shink slave panes vertically
         , ("M-i"                   , mirrorExpand           ) -- expand slave panes vertically
         , ("M-f"                   , jumpToFull             ) -- jump to full layout
         , ("M-b"                   , banishScreen LowerRight) -- banish the mouse

         -- non-standard screen navigation
         --, ("M-h"                   , focusScreen 0          ) -- focus left screen
         --, ("M-l"                   , focusScreen 1          ) -- focus rght screen
         --, ("M-S-h"                 , shrink                 ) -- shrink master (was M-h)
         --, ("M-S-l"                 , expand                 ) -- expand master (was M-l)

         -- media keys
         , ("<XF86AudioPlay>"       , spawn "mpc toggle"     ) -- play/pause mpd
         , ("<XF86AudioStop>"       , spawn "mpc stop"       ) -- stop mpd
         , ("<XF86AudioPrev>"       , spawn "mpc prev"       ) -- prev song
         , ("<XF86AudioNext>"       , spawn "mpc next"       ) -- next song
         , ("<XF86AudioMute>"       , spawn "amixer -q set Master toggle"      ) -- toggle mute
         , ("<XF86AudioLowerVolume>", spawn "amixer -q set Master 1- unmute"    ) -- volume down 
         , ("<XF86AudioRaiseVolume>", spawn "amixer -q set Master 1+ unmute"    ) -- volume up
         --, ("M-<XF86AudioPlay>"     , mplayer "pause"        ) -- play/pause mplayer
         --, ("M-<XF86AudioStop>"     , mplayer "stop"         ) -- stop mplayer
         --, ("M-<XF86AudioPrev>"     , mplayer "seek -10"     ) -- seek back 10s
         --, ("M-<XF86AudioNext>"     , mplayer "seek 10"      ) -- seek forward 10s

         -- kill, reconfigure, exit
         , ("M-C-S-c"                , killAll                ) -- close all windows on this ws
         , ("M-S-c"                , kill                ) -- close all windows on this ws
         , ("M-q"                   , myRestart              ) -- restart xmonad
         , ("M-S-q"                 , spawn "leave"          ) -- logout menu

         -- See http://pbrisbin.com/xmonad/docs/ScratchPadKeys.html
         ] ++ scratchPadKeys scratchPadList

    where

        shrink = sendMessage Shrink
        expand = sendMessage Expand

        mirrorShrink = sendMessage MirrorShrink
        mirrorExpand = sendMessage MirrorExpand

        focusScreen n = screenWorkspace n >>= flip whenJust (windows . W.view)
        jumpToFull    = sendMessage $ JumpToLayout "Hinted Full"

        myBrowser  = spawn "$BROWSER"
        myLock     = spawn "slock"
        myEject    = spawn "eject -T"
        myMail     = spawn $ myTerminal ++ " -e mutt"

        -- see http://pbrisbin.com/xmonad/docs/SendFile.html
        sendFile   = sendFilePrompt promptConfig "~/.mutt/alias"

        -- see http://pbrisbin.com/pages/screen_tricks.html
        myIRC      = spawnInScreen "irssi"
        myTorrents = spawnInScreen "rtorrent"

        spawnInScreen s = spawn $ unwords [ myTerminal, "-title", s, "-e bash -cl", command s ]

            where
                -- a quoted command to pass off to bash -cl
                command s = ("\""++) . (++"\"") $ unwords ["SCREEN_CONF=" ++ s, "screen -S", s, "-R -D", s]

        -- see http://pbrisbin.com/pages/mplayer-control.html
        mplayer s = spawn $ unwords [ "echo", s, "> $HOME/.mplayer_fifo" ]

        -- kill all conky/dzen2 before executing default restart command
        myRestart = spawn $ "for pid in `pgrep conky`; do kill -9 $pid; done && " ++
                            "for pid in `pgrep dzen2`; do kill -9 $pid; done && " ++
                            "xmonad --recompile && xmonad --restart"

-- }}}
