module Logic.Player where

import Logic.Item

import Map.Room

import Control.Lens
import qualified Data.Map as M

data Player s = Player {
    _location :: Room s,
    _inventory :: M.Map Item Int
}

makeLenses ''Player