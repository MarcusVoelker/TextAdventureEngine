module Logic.Deserialiser where

import Control.Lens

import qualified Data.Map as M

import Serialiser

import Map.Room
import Logic.EntityKind
import Logic.Item

data DeserialisationContext = DeserialisationContext {
    _deserialisationContextRooms :: M.Map String Room,
    _deserialisationContextEntityKinds :: M.Map String EntityKind,
    _deserialisationContextItems :: M.Map String Item
}

makeFields ''DeserialisationContext

instance Persistent Room DeserialisationContext where
    deserialise = do
        rs <- view rooms
        s <- deserialise
        return (rs M.! s)

instance Persistent Item DeserialisationContext where
    deserialise = do
        is <- view items
        s <- deserialise
        return (is M.! s)

instance Persistent EntityKind DeserialisationContext where
    deserialise = do
        ks <- view entityKinds
        s <- deserialise
        return (ks M.! s)
