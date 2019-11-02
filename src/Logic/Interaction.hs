module Logic.Interaction where

import Logic.GameState

import Control.Monad.Trans.State

type GameAction a = StateT GameState IO a