module Map.Room where

import Thing

import Control.Lens
import qualified Data.Map as M

data Room = Room {
    _roomIdt :: String,
    _roomName :: String,
    _roomDescription :: String,
    _roomExits :: M.Map String Room
}

makeFields ''Room

instance Eq Room where
    a == b = a^.idt == b^.idt

instance Ord Room where
    compare a b = compare (a^.idt) (b^.idt)