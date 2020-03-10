module Logic.Entity where

import Serialiser
import Thing

import Logic.EntityKind
import Logic.Deserialiser

import GameData.Room
import GameData.Text

import Control.Lens
import qualified Data.Map as M

data Entity = Entity {
    _entityIdt :: Int,
    _entityKind :: EntityKind,
    _entityState :: M.Map String Int,
    _entityLocation :: Room
}

instance Eq Entity where
    a == b = a^.idt == b^.idt

makeFields ''Entity

instance Serialisable Entity where
    serialise e = serialise (e^.idt)
        <> serialise (e^.kind)
        <> serialise (e^.state)
        <> serialise (e^.location)

instance Persistent Entity DeserialisationContext where
    deserialise = Entity <$> deserialise <*> deserialise <*> deserialise <*> deserialise

instance HasName Entity String where
    name = _entityNameLens

instance HasDescription Entity MetaText where
    description = _entityDescriptionLens
    
_entityNameLens :: Lens' Entity String
_entityNameLens = kind.name
    
_entityDescriptionLens :: Lens' Entity MetaText
_entityDescriptionLens = kind.description