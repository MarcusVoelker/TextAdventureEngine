module Actions.DefaultActions where

import Actions.GameState
import Actions.Interaction
import Map.Room

import Control.Monad.Trans.Class
import Control.Monad.Trans.State

look :: GameAction Room ()
look = do
    s <- get
    lift $ putStrLn $ description (location s) s

go :: String -> GameAction Room ()
go e = do
    s <- get
    case getExit (location s) s e of
        Left err -> lift $ putStrLn err
        Right r' -> do
            put (GameState r' (variables s))
            look