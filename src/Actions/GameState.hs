module Actions.GameState where

import qualified Data.Map as M

data GameState p = GameState {
    location :: p,
    variables :: M.Map String Int
}

initialState :: p -> [String] -> GameState p
initialState p vs = GameState p (M.fromList $ map (\v -> (v,0)) vs)