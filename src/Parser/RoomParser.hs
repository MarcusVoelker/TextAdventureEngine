module Parser.RoomParser where

import Parser.Tokenizer hiding (Keyword(Room))
import qualified Parser.Tokenizer as T (Keyword(Room))
import Parser.ObjectParser

import Map.Room
import Thing

import Control.DoubleContinuations
import Text.LParse.Parser
import Text.LParse.Prebuilt

import Control.Applicative
import Control.Monad
import Control.Lens.Getter
import Data.List
import Data.Maybe
import qualified Data.Map.Strict as M

room :: M.Map String (Room s) -> Parser r [Token] (Room s)
room rs = do
    (Object T.Room idt ps) <- object
    (SProp name) <- return $ fromMaybe (SProp idt) (lookup "name" ps)
    (SProp description) <- return $ fromMaybe (SProp "") (lookup "description" ps)
    (ListProp exits) <- return $ fromMaybe (ListProp []) (lookup "exits" ps)
    let exs = mapMaybe (\(PairProp (SProp d,SProp n)) -> (d,) <$> M.lookup n rs) exits
    return $ Room 
        idt
        name
        (const description)
        (defaultGetExit (\_ n -> lookup n exs))

rooms :: Parser r [Token] (M.Map String (Room s))
rooms = pfix $ \rs -> do
    list <- many (room rs)
    return $ M.fromList $ map (\r -> (r^.idt,r)) list