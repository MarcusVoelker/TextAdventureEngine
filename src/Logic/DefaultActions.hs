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

findEntity :: String -> GameAction (Maybe (Entity GameState))
findEntity t = do
    r <- use (player.location)
    es <- M.findWithDefault [] r <$> use entities 
    return $ find (\e -> e^.name == t) es

withEntity :: String -> (Entity GameState -> GameAction ()) -> GameAction ()
withEntity t a = do
    e <- findEntity t
    case e of 
        Nothing -> lift (putStrLn ("I cannot see any " ++ t ++ "!"))
        Just e -> a e

findItem :: String -> GameAction (Maybe Item)
findItem t = do
    inv <- use $ player.inventory
    return $ find (\i -> i^.name == t) $ M.keys inv

withItem :: String -> (Item -> GameAction ()) -> GameAction ()
withItem t a = do
    i <- findItem t
    case i of 
        Nothing -> lift (putStrLn ("I am not carrying any " ++ t ++ "!"))
        Just i -> a i

lookAt :: String -> GameAction ()
lookAt t = withEntity t $ \e -> lift $ putStrLn $ e^.description

takeItem :: String -> GameAction ()
takeItem t = withEntity t $ \e -> case e^.kind.item of 
    Nothing -> lift $ putStrLn $ "I cannot pick up " ++ t ++ "!"
    Just i -> do 
        r <- use (player.location)
        removeEntity r e 
        addToInventory i

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
            putStrLn "I am carrying nothing."
        else
            flip M.foldMapWithKey inv $ \k v -> putStrLn $ "    " ++ (k^.name) ++
                if k^.stackable then 
                    ": " ++ show v
                else
                    ""

useOn :: String -> String -> GameAction ()
useOn ti te = withEntity te $ \e -> 
    withItem ti $ \i ->
        lift $ putStrLn "I sure can use that!"