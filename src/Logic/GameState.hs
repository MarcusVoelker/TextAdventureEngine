module Logic.GameState where

import Control.Lens

import Logic.Entity
import Map.Room

import qualified Data.Map as M

data GameState = GameState {
    _playerLocation :: Room GameState,
    _variables :: M.Map String Int,
    _entities :: M.Map (Room GameState) [Entity GameState]
}

makeLenses ''GameState

initialState :: Room GameState -> [String] -> GameState
initialState r vs = GameState r (M.fromList $ map (,0) vs) M.empty