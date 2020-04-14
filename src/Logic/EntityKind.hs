module Logic.EntityKind where

import Control.Lens

import qualified Data.Map as M

import Serialiser
import Thing

import GameData.Text

import Logic.Event
import Logic.Item

data EntityKind = EntityKind {
    _entityKindIdt :: String,
    _entityKindName :: String,
    _entityKindDisplayName :: MetaText,
    _entityKindDescription :: MetaText,
    _entityKindVisible :: Bool,
    _entityKindTakenItem :: Maybe Item,
    _entityKindUseEvent :: Maybe UseEvent,
    _entityKindAccepts :: M.Map Item UseEvent
}

makeFields ''EntityKind

instance Eq EntityKind where
    a == b = a^.idt == b^.idt

instance Serialisable EntityKind where
    serialise k = serialise (k^.idt)
