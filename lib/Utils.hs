{-# OPTIONS -fno-warn-missing-signatures #-}
module Utils 
    ( 
    -- * Utilities
    cleanStart
    ) where

import XMonad

-- | Kill (@-9@) any running dzen and conky processes before executing 
--   the default restart command, this is a good @M-q@ replacement.
cleanStart :: MonadIO m => m ()
cleanStart = spawn $ "xmonad --recompile && "
                  ++ "xmonad --restart"

