module Logic.GameState where

import Control.Lens

import Logic.Entity
import Logic.Player
import Map.Room

import qualified Data.Map as M

data GameState = GameState {
    _gameStatePlayer :: Player GameState,
    _gameStateVariables :: M.Map String Int,
    _gameStateEntities :: M.Map Room [Entity],
    _gameStateNextIdt :: Int,
    _gameStateDynamicDoors :: M.Map Room [(String, Room)],
    _gameStateDynamicDescription :: M.Map Room String
}

makeFields ''GameState

initialState :: Room -> [String] -> GameState
initialState r vs = 
    GameState 
        (Player r M.empty) 
        (M.fromList $ map (,0) vs) 
        M.empty
        0
        M.empty
        M.empty