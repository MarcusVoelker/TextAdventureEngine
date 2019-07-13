module MainLoop where

import Parser.RoomParser
import Parser.Tokenizer
import Actions.GameState
import Actions.DefaultActions
import Actions.Interaction

import Text.LParse.Parser

import Control.Arrow
import Control.Monad.Trans.State
import Data.Maybe
import qualified Data.Map.Strict as M

runGame :: IO ()
runGame = do
    roomCode <- readFile "app/rooms.dat"
    let rs = doParse (tokenizer >>> blocker >>> rooms) roomCode
    case rs of
        Left x -> print x
        Right rMap -> do
            let initial = initialState (fromJust $ M.lookup "init" rMap) []
            evalStateT look initial
