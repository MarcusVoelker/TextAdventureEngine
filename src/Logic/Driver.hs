module Logic.Driver where

import Logic.Interaction
import Logic.GameState
import Logic.Response
import Logic.StateStack
import Parser.ActionParser

import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.Trans.Class

import Text.LParse.Parser

executeCommand :: String -> StateStack -> Responding StateStack
executeCommand command ss 
    | command == "quit" = Responding [QuitResponse] ss
    | otherwise = 
        if noContext ss then
            parse action command 
                (\c -> execStateT (liftBottom c) ss)
                (const $ Responding [TextResponse "I did not understand that."] ss)
        else 
            execStateT (liftTemporary (tempAction command)) ss