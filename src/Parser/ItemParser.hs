module Parser.ItemParser where

import Parser.Tokenizer hiding (Keyword(Item))
import qualified Parser.Tokenizer as T (Keyword(Item))
import Parser.ObjectParser
import Parser.TextParser

import Logic.Item
import Thing

import Text.LParse.Parser

import Control.Applicative
import Control.Arrow
import Control.Lens
import qualified Data.Map as M
import Data.Maybe

item :: Parser r [Token] Item
item = do
    (Object T.Item idt ps) <- object
    (SProp name) <- return $ fromMaybe (SProp idt) (lookup "name" ps)
    vt <- return ((\(SProp d) -> d) (fromMaybe (SProp "") (lookup "description" ps))) >>> metaText
    let stack = isJust (lookup "stackable" ps)
    return $ Item idt name vt stack

items :: Parser r [Token] (M.Map String Item)
items = do
    list <- some item
    return $ M.fromList $ map (\i -> (i^.idt,i)) list