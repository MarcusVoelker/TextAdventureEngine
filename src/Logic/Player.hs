module Logic.Player where

import Serialiser
import Thing

import Logic.Deserialiser
import Logic.Item

import GameData.Room

import Control.Lens
import qualified Data.Map as M

data Player = Player {
    _playerLocation :: Room,
    _playerInventory :: M.Map Item Int
}

makeFields ''Player

instance Serialisable Player where
    serialise p = serialise (p^.location) <> serialise (p^.inventory)

instance Persistent Player DeserialisationContext where
    deserialise = Player <$> deserialise <*> deserialise
