module Parser.ConfigParser where

import Parser.Tokenizer hiding (Keyword(Config))
import qualified Parser.Tokenizer as T (Keyword(Config))
import Parser.ObjectParser

import Text.LParse.Parser

import Data.Maybe

config :: Parser r [Token] (String,String)
config = do
    (Object T.Config "main" ps) <- object
    (SProp title) <- return $ fromMaybe (SProp "Untitled Text Adventure") (lookup "windowTitle" ps)
    (SProp iR) <- return $ fromMaybe (SProp "init") (lookup "initialRoom" ps)
    return (title,iR)