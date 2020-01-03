module Logic.Entity where

import Logic.Dialogue
import Logic.Item
import Thing

import Map.Room

import Control.Lens
import Control.Lens.Lens
import qualified Data.Map as M

data UseEvent = UnlockDoor {
    _useEventDirection :: String,
    _useEventItem :: Item,
    _useEventNewRoom :: Room
}

newtype TalkEvent = TalkEvent {
    _talkEventDialogueTree :: DialogueTree
}

data EntityKind = EntityKind {
    _entityKindIdt :: String,
    _entityKindName :: String,
    _entityKindDescription :: String,
    _entityKindVisible :: Bool,
    _entityKindItem :: Maybe Item,
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

makeFields ''UseEvent
makeFields ''TalkEvent
makeFields ''EntityKind
makeFields ''Entity

instance HasName Entity String where
    name = _entityNameLens
instance HasDescription Entity String where
    description = _entityDescriptionLens

_entityNameLens :: Lens' Entity String
_entityNameLens = kind.name

_entityDescriptionLens :: Lens' Entity String
_entityDescriptionLens = kind.description