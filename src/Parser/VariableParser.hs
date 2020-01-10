module Parser.VariableParser where

import Parser.Tokenizer hiding (Keyword(Variable))
import qualified Parser.Tokenizer as T (Keyword(Variable))
import Parser.ObjectParser

import Text.LParse.Parser

import Control.Applicative
import qualified Data.Map as M
import Data.Maybe

variable :: Parser r [Token] (String,Int)
variable = do
    (Object T.Variable idt ps) <- object
    (SProp iv) <- return $ fromMaybe (SProp "0") (lookup "init" ps)
    return (idt,read iv)

variables :: Parser r [Token] (M.Map String Int)
variables = do
    list <- some variable
    return $ M.fromList list