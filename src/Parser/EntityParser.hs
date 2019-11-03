module Parser.EntityParser where

import Parser.Tokenizer hiding (Keyword(Room), Keyword(Entity))
import qualified Parser.Tokenizer as T (Keyword(Room), Keyword(Entity))
import Parser.ObjectParser

import Logic.Entity
import Map.Room

import Control.DoubleContinuations
import Text.LParse.Parser
import Text.LParse.Prebuilt

import Control.Applicative
import Control.Monad
import Data.List
import qualified Data.Map as M
import Data.Maybe

entity :: M.Map String (Room s) -> Parser r [Token] (Maybe (Room s),EntityKind)
entity rs = do
    (Object T.Entity idt ps) <- object
    (SProp name) <- return $ fromMaybe (SProp idt) (lookup "name" ps)
    (SProp description) <- return $ fromMaybe (SProp "") (lookup "description" ps)
    let room = lookup "location" ps >>= (\(SProp rName) -> rs M.!? rName)
    return (room,EntityKind idt name description True)

entities :: M.Map String (Room s) -> Parser r [Token] [(Maybe (Room s),EntityKind)]
entities = many . entity