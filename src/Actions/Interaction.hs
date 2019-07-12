module Actions.Interaction where

import Actions.GameState

import Control.Monad.Trans.State

type GameAction p a = StateT (GameState p) IO a