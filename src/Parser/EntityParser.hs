module Parser.EntityParser where

import Parser.Tokenizer hiding (Keyword(Room))
import qualified Parser.Tokenizer as T (Keyword(Room))
import Parser.ObjectParser

import Logic.Entity
import Map.Room

import Control.DoubleContinuations
import Text.LParse.Parser
import Text.LParse.Prebuilt

import Control.Applicative
import Control.Monad
import Data.List
import Data.Maybe

entity :: Parser r [Token] EntityKind
entity = do
    (Object T.Room idt ps) <- object
    (SProp name) <- return $ fromMaybe (SProp idt) (lookup "name" ps)
    (SProp description) <- return $ fromMaybe (SProp "") (lookup "description" ps)
    return $ EntityKind idt name description True