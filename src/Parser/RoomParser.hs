module Parser.RoomParser where

import Parser.Tokenizer hiding (Keyword(Room))
import qualified Parser.Tokenizer as T (Keyword(Room))
import Parser.ObjectParser
import Parser.TextParser

import GameData.Room
import Thing

import Text.LParse.Parser

import Control.Applicative
import Control.Arrow
import Control.Lens.Getter
import Data.Maybe
import qualified Data.Map.Strict as M

room :: M.Map String Room -> Parser r [Token] Room
room rs = do
    (Object T.Room idt ps) <- object
    (SProp name) <- return $ fromMaybe (SProp idt) (lookup "name" ps)
    vt <- return ((\(SProp d) -> d) (fromMaybe (SProp "") (lookup "description" ps))) >>> metaText
    (ListProp exits) <- return $ fromMaybe (ListProp []) (lookup "exits" ps)
    let exs = mapMaybe (\(PairProp (SProp d,SProp n)) -> (d,) <$> M.lookup n rs) exits
    return $ Room 
        idt
        name
        vt
        (M.fromList exs)

rooms :: Parser r [Token] (M.Map String Room)
rooms = pfix $ \rs -> do
    list <- many (room rs)
    return $ M.fromList $ map (\r -> (r^.idt,r)) list