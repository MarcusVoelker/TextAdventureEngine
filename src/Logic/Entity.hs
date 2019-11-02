module Logic.Entity where

import Map.Room hiding (_name)

import Control.Lens
import qualified Data.Map as M

data Entity s = Entity {
    _name :: String,
    _description :: String,
    _visible :: Bool,
    _state :: M.Map String Int,
    _location :: Room s
}

instance Eq (Entity s) where
    a == b = _name a == _name b

makeLenses ''Entity