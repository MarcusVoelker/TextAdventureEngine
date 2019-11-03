module Logic.Interaction where

import Logic.GameState
import Logic.Player
import Logic.Item
import Logic.Entity

import Map.Room

import Control.Lens.Setter
import Control.Monad.Trans.State

import Data.List
import qualified Data.Map as M

type GameAction a = StateT GameState IO a

addToInventory :: Item -> GameAction ()
addToInventory i = player.inventory %= M.insertWith (+) i 1

removeEntity :: Room GameState -> Entity GameState -> GameAction ()
removeEntity r e = entities %= M.adjust (delete e) r