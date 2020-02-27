module Logic.Item where

import GameData.Text

import Thing

import Control.Lens

data Item = Item {
    _itemIdt :: String,
    _itemName :: String,
    _itemDisplayName :: MetaText,
    _itemDescription :: MetaText,
    _itemStackable :: Bool
}

instance Eq Item where
    a == b = a^.idt == b^.idt

instance Ord Item where
    compare a b = compare (a^.idt) (b^.idt)

makeFields ''Item