module Logic.DefaultActions where

import Logic.Entity
import Logic.GameState
import Logic.Interaction
import Map.Room hiding (name,description)
import qualified Map.Room as Room (name,description)

import Control.Lens
import Control.Lens.Getter
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import qualified Data.Map as M

look :: GameAction ()
look = do
    s <- get
    r <- use playerLocation
    lift $ putStrLn $ (r^.Room.description) s
    es <- M.findWithDefault [] r <$> use entities 
    lift $ putStrLn "\nYou see here:"
    lift $ forM_ es $ \e -> do
        putStr "    "
        print $ e^.name
        putStrLn ""

lookAt :: String -> GameAction ()
lookAt t = do
    s <- get
    r <- use playerLocation
    lift $ putStrLn $ (r^.objDescription) s t

go :: String -> GameAction ()
go e = do
    s <- get
    r <- use playerLocation
    case (r^.getExit) s e of
        Left err -> lift $ putStrLn err
        Right r' -> do
            playerLocation .= r'
            look