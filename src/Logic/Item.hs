module Logic.Item where

import Control.Lens

data Item = Item {
    _idt :: String,
    _name :: String,
    _description :: String,
    _stackable :: Bool
} deriving (Eq,Ord)

makeLenses ''Item