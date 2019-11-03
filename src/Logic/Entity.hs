module Logic.Entity where

import Map.Room hiding (_idt)

import Control.Lens
import qualified Data.Map as M

data Entity s = Entity {
    _idt :: String,
    _name :: String,
    _description :: String,
    _visible :: Bool,
    _state :: M.Map String Int,
    _location :: Room s
}

instance Eq (Entity s) where
    a == b = _idt a == _idt b

makeLenses ''Entity