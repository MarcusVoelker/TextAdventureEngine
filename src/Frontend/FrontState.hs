module Frontend.FrontState where

import Frontend.State

import Logic.Response

import Control.Exception
import Control.Lens
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader

type FrontMod a = StateT FrontendState IO a
type FrontRead a = ReaderT FrontendState IO a
type RespondingFrontMod a = RespondingT (StateT FrontendState IO) a

lift2 :: IO a -> RespondingFrontMod a
lift2 = lift . lift

resolveLocation :: LocKind -> ScreenLoc -> FrontRead Int
resolveLocation _ (Absolute x) | x >= 0 = return x
resolveLocation X (Absolute x) = do
        max <- views (settings.dimensions) fst
        return $ max + x
resolveLocation Y (Absolute x) = do
        max <- views (settings.dimensions) snd
        return $ max + x
resolveLocation _ _  = lift $ throwIO $ PatternMatchFail "Relative locations not implemented!"

liftRead :: FrontRead a -> FrontMod a
liftRead fr = StateT $ \e -> fmap (,e) (runReaderT fr e)