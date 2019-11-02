module Logic.GameState where

import Control.Lens

import Logic.Entity

import qualified Data.Map as M

data GameState p = GameState {
    _playerLocation :: p,
    _variables :: M.Map String Int,
    _entities :: M.Map p [Entity p]
}

makeLenses ''GameState

initialState :: p -> [String] -> GameState p
initialState p vs = GameState p (M.fromList $ map (,0) vs) M.empty