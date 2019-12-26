module Parser.ConfigParser where

import Parser.Tokenizer hiding (Keyword(Config))
import qualified Parser.Tokenizer as T (Keyword(Config))
import Parser.ObjectParser

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

config :: Parser r [Token] String
config = do
    (Object T.Config "main" ps) <- object
    (SProp title) <- return $ fromMaybe (SProp "Untitled Text Adventure") (lookup "windowTitle" ps)
    return title