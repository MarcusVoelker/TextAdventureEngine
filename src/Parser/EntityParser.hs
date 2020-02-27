module Parser.EntityParser where

import Parser.Tokenizer hiding (Keyword(Entity))
import qualified Parser.Tokenizer as T (Keyword(Entity))
import Parser.ObjectParser
import Parser.TextParser

import Logic.Entity
import Logic.Event
import Logic.Item
import GameData.Room

import Text.LParse.Parser

import Control.Applicative
import Control.Arrow
import qualified Data.Map as M
import Data.Maybe

accept :: M.Map String Room -> M.Map String Item -> String -> String -> String -> Maybe String -> Maybe (Item,UseEvent)
accept rs is dir rName iName resp = do
    r <- rs M.!? rName
    i <- is M.!? iName
    return (i,UnlockDoor dir i r resp)

entity :: M.Map String Room -> M.Map String Item -> Parser r [Token] (Maybe Room,EntityKind)
entity rs is = do
    (Object T.Entity idt ps) <- object
    (SProp name) <- return $ fromMaybe (SProp idt) (lookup "name" ps)
    dn <- return ((\(SProp d) -> d) (fromMaybe (SProp name) (lookup "displayName" ps))) >>> metaText
    vt <- return ((\(SProp d) -> d) (fromMaybe (SProp "") (lookup "description" ps))) >>> metaText
    let item = lookup "item" ps >>= (\(SProp iName) -> is M.!? iName)
    let room = lookup "location" ps >>= (\(SProp rName) -> rs M.!? rName)
    let ue = (\case 
            (PairProp (SProp "displayText",SProp s)) -> DisplayText s
            _ -> GenericUseEvent
            ) <$> lookup "useEvent" ps
    (ListProp acs) <- return $ fromMaybe (ListProp []) (lookup "accepts" ps)
    let accepts = M.fromList $ mapMaybe (\case
            (PairProp (SProp i, PairProp (SProp "unlockDoor", PairProp (SProp dir, SProp r)))) -> accept rs is dir r i Nothing
            (PairProp (SProp i, PairProp (SProp "unlockDoor", PairProp (PairProp (SProp dir, SProp r),SProp text)))) -> accept rs is dir r i (Just text)
            _ -> undefined
            ) acs 
    return (room,EntityKind idt name dn vt True item ue accepts)

entities :: M.Map String Room -> M.Map String Item -> Parser r [Token] [(Maybe Room,EntityKind)]
entities rs is = many $ entity rs is