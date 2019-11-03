module Logic.GameState where

import Control.Lens

import Logic.Entity
import Logic.Player
import Map.Room

import qualified Data.Map as M

data GameState = GameState {
    _gameStatePlayer :: Player GameState,
    _gameStateVariables :: M.Map String Int,
    _gameStateEntities :: M.Map (Room GameState) [Entity GameState]
}

makeFields ''GameState

initialState :: Room GameState -> [(Room GameState,EntityKind)] -> [String] -> GameState
initialState r es vs = GameState (Player r M.empty) (M.fromList $ map (,0) vs) $ 
    M.fromListWith (++) $ map (\(r,k) -> (r,[Entity "k" k M.empty r])) es