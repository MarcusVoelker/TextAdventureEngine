module Logic.Driver where

import Logic.Response
import Logic.StateStack
import Parser.ActionParser

import GameData.Text

import Control.Monad.Trans.State

import Text.LParse.Parser

executeCommand :: String -> GameStepper Responding
executeCommand command = step1 $ \gs -> parse action command 
        (`execStateT` gs)
        (const $ Responding [TextResponse $ liftString "I did not understand that."] gs)