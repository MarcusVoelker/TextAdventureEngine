module Logic.Entity where

import Logic.Item
import Thing

import Map.Room

import Control.Lens
import Control.Lens.Lens
import qualified Data.Map as M

data EntityKind = EntityKind {
    _entityKindIdt :: String,
    _entityKindName :: String,
    _entityKindDescription :: String,
    _entityKindVisible :: Bool,
    _entityKindItem :: Maybe Item
}

data Entity s =  Entity {
    _entityIdt :: String,
    _entityKind :: EntityKind,
    _entityState :: M.Map String Int,
    _entityLocation :: Room s
}

instance Eq EntityKind where
    a == b = a^.idt == b^.idt

instance Eq (Entity s) where
    a == b = a^.idt == b^.idt

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