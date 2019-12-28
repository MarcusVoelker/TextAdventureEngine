module Frontend.TAIO where

import Frontend.State

import Control.Lens
import Control.Monad.Trans.Class
import Control.Monad.Trans.State


type TAIO a = StateT FrontendState IO a

resolveLocation :: LocKind -> ScreenLoc -> TAIO Int
resolveLocation _ (Absolute x) | x >= 0 = return x
resolveLocation X (Absolute x) = do
        max <- uses (settings.dimensions) fst
        return $ max + x
resolveLocation Y (Absolute x) = do
        max <- uses (settings.dimensions) snd
        return $ max + x