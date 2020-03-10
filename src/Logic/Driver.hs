module Logic.Driver where

import Logic.Response
import Logic.StateStack
import Parser.ActionParser

import GameData.Text

import Control.Monad.Trans.State

import Text.LParse.Parser

executeCommand :: String -> StateStack -> Responding StateStack
executeCommand command ss = 
    if noContext ss then
        parse action command 
            (\c -> execStateT (liftBottom c) ss)
            (const $ Responding [TextResponse $ liftString "I did not understand that."] ss)
    else 
        execStateT (liftTemporary (tempAction command)) ss