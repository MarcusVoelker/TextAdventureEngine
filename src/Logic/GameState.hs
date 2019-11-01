module Logic.GameState where

import Control.Lens

import Logic.Entity

import qualified Data.Map as M

data GameState p = GameState {
    _location :: p,
    _variables :: M.Map String Int,
    _entities :: M.Map p [Entity]
}

makeLenses ''GameState

initialState :: p -> [String] -> GameState p
initialState p vs = GameState p (M.fromList $ map (\v -> (v,0)) vs) M.empty