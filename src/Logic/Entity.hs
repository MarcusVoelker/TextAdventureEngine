module Logic.Entity where

import Control.Lens
import qualified Data.Map as M

data Entity p = Entity {
    _name :: String,
    _description :: String,
    _visible :: Bool,
    _state :: M.Map String Int,
    _location :: p
} deriving Eq

makeLenses ''Entity