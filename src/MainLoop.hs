module MainLoop where

import Parser.ConfigParser
import Parser.EntityParser
import Parser.ItemParser
import Parser.RoomParser
import Parser.VariableParser
import Parser.Tokenizer
import Logic.Deserialiser (DeserialisationContext,DeserialisationContext(DeserialisationContext))
import Logic.EntityKind
import Logic.GameState
import Logic.Interaction
import Logic.Response
import Logic.StateStack
import Sound.Engine
import Engine
import Thing
import Frontend.Frontend

import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Interface.Environment

import Text.LParse.Parser

import Control.Arrow hiding (left)
import Control.DoubleContinuations
import Control.Lens hiding (view)
import Control.Monad.Trans.State
import Data.Maybe
import qualified Data.Map.Strict as M

entityMap :: [EntityKind] -> M.Map String EntityKind
entityMap = M.fromList . map (\ek -> (ek^.idt,ek))

fullParser :: String -> String -> String -> String -> String -> DCont r String (Maybe (Int,Int),String,StateStack,DeserialisationContext)
fullParser r e i c v = do
    rMap <- fst <$> pFunc (tokenizer >>> blocker >>> rooms) r
    iMap <- fst <$> pFunc (tokenizer >>> blocker >>> items) i
    es <- fst <$> pFunc (tokenizer >>> blocker >>> Parser.EntityParser.entities rMap iMap) e
    (d,t,iR) <- fst <$> pFunc (tokenizer >>> blocker >>> config) c
    vs <- fst <$> pFunc (tokenizer >>> blocker >>> Parser.VariableParser.variables) v
    let initial = initialState (fromJust $ M.lookup iR rMap) vs
    return $ (d,t,,DeserialisationContext rMap (entityMap $ map snd es) iMap) $ (^.result) $ (\gs -> StateStack gs []) <$> execStateT (mapM_ (uncurry instantiateEntity) (mapMaybe (\(r,e) -> (,e) <$> r) es)) initial

runGame :: IO ()
runGame = withEngine soundEngine $ do
    roomCode <- readFile "app/rooms.dat"
    entityCode <- readFile "app/entities.dat"
    itemCode <- readFile "app/items.dat"
    configCode <- readFile "app/config.dat"
    variableCode <- readFile "app/variables.dat"
    run (fullParser roomCode entityCode itemCode configCode variableCode) (\(d,t,ss,dc) -> mainOpenGL d t ss dc) putStrLn

mainOpenGL :: Maybe (Int,Int) -> String -> StateStack -> DeserialisationContext -> IO ()
mainOpenGL dims title ss dc = do
    (sw,sh) <- getScreenSize
    let (disp,ifs) = case dims of
            Just (w,h) -> (InWindow title (w+1,h+1) (div(sw-w)2,div(sh-h)2),initialFrontendState (div w 8,div h 16) (8,16) dc)
            Nothing -> (FullScreen,initialFrontendState (div sw 8,div sh 16) (8,16) dc)
    playIO
        disp
        black
        60
        (ss,ifs)
        renderHandler
        eventHandler
        updateHandler