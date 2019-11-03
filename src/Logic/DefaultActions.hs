module Logic.DefaultActions where

import Logic.Entity
import Logic.GameState
import Logic.Interaction
import Logic.Player
import Logic.Item

import Map.Room
import Thing

import Control.Lens
import Control.Lens.Getter
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.List
import qualified Data.Map as M

look :: GameAction ()
look = do
    s <- get
    r <- use (player.location)
    lift $ putStrLn $ (r^.description) s
    es <- M.findWithDefault [] r <$> use entities 
    unless (null es) $ lift $ do
        putStrLn "\nYou see here:"
        forM_ es $ \e -> putStrLn $ "    " ++ (e^.name)

lookAt :: String -> GameAction ()
lookAt t = do
    r <- use (player.location)
    es <- M.findWithDefault [] r <$> use entities 
    let e = find (\e -> e^.name == t) es
    lift $ case e of 
        Nothing -> putStrLn $ "I cannot see any " ++ t ++ "!"
        Just e -> putStrLn $ e^.description

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
                print $ k^.name
                putStr ": "
                print v
                putStrLn ""