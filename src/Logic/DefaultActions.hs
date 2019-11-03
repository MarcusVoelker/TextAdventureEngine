module Logic.DefaultActions where

import Logic.Entity hiding (location)
import Logic.GameState
import Logic.Interaction
import Logic.Player
import Logic.Item hiding (name)
import qualified Logic.Item as I (name)
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
    r <- use (player.location)
    lift $ putStrLn $ (r^.Room.description) s
    es <- M.findWithDefault [] r <$> use entities 
    unless (null es) $ do
        lift $ putStrLn "\nYou see here:"
        lift $ forM_ es $ \e -> do
            putStr "    "
            print $ e^.name
            putStrLn ""

lookAt :: String -> GameAction ()
lookAt t = do
    s <- get
    r <- use (player.location)
    lift $ putStrLn $ (r^.objDescription) s t

go :: String -> GameAction ()
go e = do
    s <- get
    r <- use (player.location)
    case (r^.getExit) s e of
        Left err -> lift $ putStrLn err
        Right r' -> do
            (player.location) .= r'
            look

viewInv :: GameAction ()
viewInv = do
    inv <- use (player.inventory)
    lift $ 
        if M.null inv then
            putStrLn "You are carrying nothing."
        else
            flip M.foldMapWithKey inv $ \k v -> do
                putStr "    "
                print $ k^.I.name
                putStr ": "
                print v
                putStrLn ""