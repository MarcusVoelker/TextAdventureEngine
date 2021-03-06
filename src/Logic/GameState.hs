module Logic.GameState where

import Control.Lens

import Logic.Deserialiser
import Logic.Entity
import Logic.Player
import GameData.Room
import GameData.Text

import Serialiser

import qualified Data.Map as M

data GameState = GameState {
    _gameStatePlayer :: Player,
    _gameStateVariables :: M.Map String Int,
    _gameStateEntities :: M.Map Room [Entity],
    _gameStateNextIdt :: Int,
    _gameStateDynamicDoors :: M.Map Room [(String, Room)],
    _gameStateDynamicDescription :: M.Map Room MetaText
}

makeFields ''GameState

instance Serialisable GameState where
    serialise gs = serialise (gs^.player)
        <> serialise (gs^.variables)
        <> serialise (gs^.entities)
        <> serialise (gs^.nextIdt)
        <> serialise (gs^.dynamicDoors)
        <> serialise (gs^.dynamicDescription)

instance Persistent GameState DeserialisationContext where
    deserialise = GameState <$>
        deserialise <*>
        deserialise <*>
        deserialise <*>
        deserialise <*>
        deserialise <*>
        deserialise

initialState :: Room -> M.Map String Int -> GameState
initialState r vs = 
    GameState 
        (Player r M.empty) 
        vs 
        M.empty
        0
        M.empty
        M.empty