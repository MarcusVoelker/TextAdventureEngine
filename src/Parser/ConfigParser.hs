module Parser.ConfigParser where

import Parser.Tokenizer hiding (Keyword(Config))
import qualified Parser.Tokenizer as T (Keyword(Config))
import Parser.ObjectParser

import Text.LParse.Parser

import Data.Maybe

config :: Parser r [Token] (Maybe (Int,Int),String,String)
config = do
    (Object T.Config "main" ps) <- object
    let dims = case lookup "windowSize" ps of
            Just (SProp "full") -> Nothing
            Just (PairProp (SProp x, SProp y)) -> Just (read x,read y)
            _ -> Just (960,640)
    (SProp title) <- return $ fromMaybe (SProp "Untitled Text Adventure") (lookup "windowTitle" ps)
    (SProp iR) <- return $ fromMaybe (SProp "init") (lookup "initialRoom" ps)
    return (dims,title,iR)