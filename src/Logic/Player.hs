module Logic.Player where

import Thing

import Logic.Item

import Map.Room

import Control.Lens
import qualified Data.Map as M

data Player = Player {
    _playerLocation :: Room,
    _playerInventory :: M.Map Item Int
}

makeFields ''Player

