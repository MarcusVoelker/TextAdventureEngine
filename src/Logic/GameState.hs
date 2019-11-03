module Logic.GameState where

import Control.Lens

import Logic.Entity
import Logic.Player
import Map.Room

import qualified Data.Map as M

data GameState = GameState {
    _player :: Player GameState,
    _variables :: M.Map String Int,
    _entities :: M.Map (Room GameState) [Entity GameState]
}

makeLenses ''GameState

initialState :: Room GameState -> [String] -> GameState
initialState r vs = GameState (Player r M.empty) (M.fromList $ map (,0) vs) M.empty