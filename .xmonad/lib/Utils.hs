{-# OPTIONS -fno-warn-missing-signatures #-}
module Utils 
    ( 
    -- * Utilities
    hideNSP
    , matchAny
    , cleanStart
    ) where

import XMonad
import XMonad.Hooks.ManageHelpers   (isDialog, isFullscreen, doFullFloat, doCenterFloat)
import XMonad.Hooks.DynamicLog      (PP(..), pad)

-- | Hide the "NSP" workspace.
hideNSP :: WorkspaceId -> String
hideNSP ws = if ws /= "NSP" then pad ws else ""

-- | Match a string against any one of a window's class, title, name or 
--   role.
matchAny :: String -> Query Bool
matchAny x = foldr ((<||>) . (=? x)) (return False) [className, title, name, role]

-- | Match against @WM_NAME@.
name :: Query String
name = stringProperty "WM_NAME"

-- | Match against @WM_ROLE@.
role :: Query String
role = stringProperty "WM_ROLE"

-- | Kill (@-9@) any running dzen and conky processes before executing 
--   the default restart command, this is a good @M-q@ replacement.
cleanStart :: MonadIO m => m ()
cleanStart = spawn $ "xmonad --recompile && "
                  ++ "xmonad --restart"

