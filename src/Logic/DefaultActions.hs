module Logic.DefaultActions where

import Logic.Dialogue
import Logic.Entity
import Logic.GameState
import Logic.Interaction
import Logic.Player
import Logic.Item
import Logic.Response

import Map.Room
import Thing

import Control.Lens
import Control.Lens.Getter
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.List
import qualified Data.Map as M
import Data.Maybe

roomDescription :: Room -> GameAction String
roomDescription r = do
    desc <- (M.!? r) <$> use dynamicDescription
    return $ fromMaybe (r^.description) desc

look :: GameAction ()
look = do
    r <- use (player.location) 
    roomDescription r >>= respondText
    es <- M.findWithDefault [] r <$> use entities 
    dyns <- use dynamicDoors
    let exs = M.keys (r^.exits) ++ map fst (fromMaybe [] (dyns M.!? r))
    unless (null exs) $ if length exs == 1 then respondText ("\nThere is an exit to the " ++ unwords exs) else respondText ("\nThere are exits to the " ++ intercalate ", " exs)
    unless (null es) $ do
        respondText "\nYou see here:"
        forM_ es $ \e -> respondText $ "    " ++ (e^.name)

findEntity :: String -> GameAction (Maybe Entity)
findEntity t = do
    r <- use (player.location)
    es <- M.findWithDefault [] r <$> use entities 
    return $ find (\e -> e^.name == t) es

withEntity :: String -> (Entity -> GameAction ()) -> GameAction ()
withEntity t a = do
    e <- findEntity t
    case e of 
        Nothing -> respondText $ "I cannot see any " ++ t ++ "!"
        Just e -> a e

findItem :: String -> GameAction (Maybe Item)
findItem t = do
    inv <- use $ player.inventory
    return $ find (\i -> i^.name == t) $ M.keys inv

withItem :: String -> (Item -> GameAction ()) -> GameAction ()
withItem t a = do
    i <- findItem t
    case i of 
        Nothing -> respondText $ "I am not carrying any " ++ t ++ "!"
        Just i -> a i

lookAt :: String -> GameAction ()
lookAt t = withEntity t $ \e -> respondText $ e^.description

takeItem :: String -> GameAction ()
takeItem t = withEntity t $ \e -> case e^.kind.item of 
    Nothing -> respondText $ "I cannot pick up " ++ t ++ "!"
    Just i -> do 
        r <- use (player.location)
        removeEntity r e 
        addToInventory i

talkTo :: String -> GameAction ()
talkTo t = withEntity t $ const $ respond $ InitiateDialogueResponse (DialogueTree "Hello there!" $ M.fromList [("bye",Nothing),("hi",Just (DialogueTree "Go away." $ M.singleton "bye" Nothing))])

exit :: Room -> String -> GameAction (Either String Room)
exit r s = do
    let s' = case s of
            "n" -> "north"
            "s" -> "south"
            "w" -> "west"
            "e" -> "east"
            "u" -> "up"
            "d" -> "down"
            _ -> s
    dyns <- use dynamicDoors
    case dyns M.!? r >>= lookup s' of
        Just t -> return $ Right t
        Nothing -> case (r^.exits) M.!? s' of
            Just r' -> return $ Right r'
            Nothing -> return $ Left ("I see no way to go " ++ s')

go :: String -> GameAction ()
go e = do
    r <- use (player.location)
    res <- exit r e
    case res of
        Right r' -> (player.location) .= r' >> look
        Left err -> respondText err

viewInv :: GameAction ()
viewInv = do
    inv <- use (player.inventory)
    if M.null inv then
        respondText "I am carrying nothing."
    else
        responds $ flip M.foldMapWithKey inv $ \k v -> [TextResponse $ "    " ++ (k^.name) ++
            if k^.stackable then 
                ": " ++ show v
            else
                ""
            ]

addDoor :: Room -> String -> Room -> GameAction ()
addDoor s n t = dynamicDoors %= M.insertWith (++) s [(n,t)]

runEvent :: UseEvent -> Item -> Entity -> GameAction ()
runEvent (UnlockDoor dir key target) item entity 
    | key == item = do
        r <- use (player.location)
        removeEntity r entity
        addDoor r dir target
    | otherwise = respondText "This doesn't fit!"

useOn :: String -> String -> GameAction ()
useOn ti te = withEntity te $ \e -> 
    withItem ti $ \i -> 
        case (e^.kind.accepts) M.!? i of
            Nothing -> respondText $ "I see no way to use " ++ ti ++ " on " ++ te ++ "!"
            Just event -> runEvent event i e