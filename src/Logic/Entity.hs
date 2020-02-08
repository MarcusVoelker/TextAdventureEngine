module Logic.Entity where

import Logic.Event
import Logic.Item
import Thing

import GameData.Room
import GameData.Text

import Control.Lens
import qualified Data.Map as M

data EntityKind = EntityKind {
    _entityKindIdt :: String,
    _entityKindName :: String,
    _entityKindDescription :: VariadicText,
    _entityKindVisible :: Bool,
    _entityKindTakenItem :: Maybe Item,
    _entityKindUseEvent :: Maybe UseEvent,
    _entityKindAccepts :: M.Map Item UseEvent
}

data Entity = Entity {
    _entityIdt :: Int,
    _entityKind :: EntityKind,
    _entityState :: M.Map String Int,
    _entityLocation :: Room
}

instance Eq EntityKind where
    a == b = a^.idt == b^.idt

instance Eq Entity where
    a == b = a^.idt == b^.idt

makeFields ''EntityKind
makeFields ''Entity

instance HasName Entity String where
    name = _entityNameLens

instance HasDescription Entity VariadicText where
    description = _entityDescriptionLens
    
_entityNameLens :: Lens' Entity String
_entityNameLens = kind.name
    
_entityDescriptionLens :: Lens' Entity VariadicText
_entityDescriptionLens = kind.description