module Logic.EntityAction where

import Logic.Entity
import Logic.GameState
import Logic.Interaction

import Control.Lens
import Control.Monad.Trans.State hiding (state)
import Data.List
import qualified Data.Map as M

data EntityState p = EntityState {
    _elocation :: p,
    _varState :: M.Map String Int,
    _globalState :: M.Map String Int
}

makeLenses ''EntityState

type EntityAction p a = StateT (EntityState p) IO a

liftE :: (Eq p, Ord p) => Entity p -> EntityAction p a -> GameAction p a
liftE e = zoom $ lens 
    (\gs -> EntityState (e^.location) (e^.state) (gs^.variables))
    (\gs es -> gs 
        & variables .~ es^.globalState 
        & entities %~ (M.insertWith (++) (es^.elocation) [e] . M.adjust (delete e) (e^.location)))