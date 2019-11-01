module Logic.Entity where

import Control.Lens

data Entity = Entity {
    _name :: String,
    _description :: String,
    _visible :: Bool
}

makeLenses ''Entity