module Logic.EntityAction where

import Logic.Entity
import Logic.GameState
import Logic.Interaction

import Map.Room

import Control.Lens
import Control.Monad.Trans.State hiding (state)
import Data.List
import qualified Data.Map as M

data EntityState = EntityState {
    _elocation :: Room GameState,
    _varState :: M.Map String Int,
    _globalState :: M.Map String Int
}

makeLenses ''EntityState

type EntityAction a = StateT EntityState IO a

liftE :: Entity GameState -> EntityAction a -> GameAction a
liftE e = zoom $ lens 
    (\gs -> EntityState (e^.location) (e^.state) (gs^.variables))
    (\gs es -> gs 
        & variables .~ es^.globalState 
        & entities %~ (M.insertWith (++) (es^.elocation) [e] . M.adjust (delete e) (e^.location)))