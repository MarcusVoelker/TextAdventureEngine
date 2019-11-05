module Logic.Entity where

import Logic.Item
import Thing

import Map.Room

import Control.Lens
import Control.Lens.Lens
import qualified Data.Map as M

data UseEvent s = UnlockDoor {
    _useEventItem :: Item,
    _useEventNewRoom :: Room s
}

data EntityKind s = EntityKind {
    _entityKindIdt :: String,
    _entityKindName :: String,
    _entityKindDescription :: String,
    _entityKindVisible :: Bool,
    _entityKindItem :: Maybe Item,
    _entityKindAccepts :: M.Map Item (UseEvent s)
}

data Entity s = Entity {
    _entityIdt :: Int,
    _entityKind :: EntityKind s,
    _entityState :: M.Map String Int,
    _entityLocation :: Room s
}

instance Eq (EntityKind s) where
    a == b = a^.idt == b^.idt

instance Eq (Entity s) where
    a == b = a^.idt == b^.idt

makeFields ''UseEvent
makeFields ''EntityKind
makeFields ''Entity

instance HasName (Entity s) String where
    name = _entityNameLens
instance HasDescription (Entity s) String where
    description = _entityDescriptionLens

_entityNameLens :: Lens' (Entity s) String
_entityNameLens = kind.name

_entityDescriptionLens :: Lens' (Entity s) String
_entityDescriptionLens = kind.description