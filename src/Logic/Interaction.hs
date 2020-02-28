module Logic.Interaction where

import Logic.GameState
import Logic.Player
import Logic.Item
import Logic.Entity
import Logic.EntityKind
import Logic.Response

import Map.Room

import Control.Lens.Setter
import Control.Lens.Getter
import Control.Monad.Trans.State

import Data.List
import qualified Data.Map as M

type GameAction a = StateT GameState Responding a

getFreeIdt :: GameAction Int
getFreeIdt = do
    i <- use nextIdt
    nextIdt %= (+1)
    return i

instantiateEntity :: Room -> EntityKind -> GameAction ()
instantiateEntity r k = do
    i <- getFreeIdt
    entities %= M.insertWith (++) r [Entity i k M.empty r]

addToInventory :: Item -> GameAction ()
addToInventory i = player.inventory %= M.insertWith (+) i 1

removeEntity :: Room -> Entity -> GameAction ()
removeEntity r e = entities %= M.adjust (delete e) r