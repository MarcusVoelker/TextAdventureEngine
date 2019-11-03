module Parser.ItemParser where

import Parser.Tokenizer hiding (Keyword(Item))
import qualified Parser.Tokenizer as T (Keyword(Item))
import Parser.ObjectParser

import Logic.Item
import Map.Room
import Thing

import Control.DoubleContinuations
import Text.LParse.Parser
import Text.LParse.Prebuilt

import Control.Applicative
import Control.Monad
import Control.Lens
import Data.List
import qualified Data.Map as M
import Data.Maybe

item :: Parser r [Token] Item
item = do
    (Object T.Item idt ps) <- object
    (SProp name) <- return $ fromMaybe (SProp idt) (lookup "name" ps)
    (SProp description) <- return $ fromMaybe (SProp "") (lookup "description" ps)
    let stack = isJust (lookup "stackable" ps)
    return $ Item idt name description stack

items :: Parser r [Token] (M.Map String Item)
items = do
    list <- some item
    return $ M.fromList $ map (\i -> (i^.idt,i)) list