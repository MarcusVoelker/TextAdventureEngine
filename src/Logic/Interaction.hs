module Logic.Interaction where

import Logic.GameState

import Control.Monad.Trans.State

type GameAction p a = StateT (GameState p) IO a