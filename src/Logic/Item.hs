module Logic.Item where

import Serialiser
import Thing

import Control.Lens

data Item = Item {
    _itemIdt :: String,
    _itemName :: String,
    _itemDescription :: String,
    _itemStackable :: Bool
} deriving (Eq,Ord)

makeFields ''Item

instance Serialisable Item where
    serialise i = serialise (i^.idt)