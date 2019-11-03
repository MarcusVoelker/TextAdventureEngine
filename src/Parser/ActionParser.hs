module Parser.ActionParser where

import Logic.GameState
import Logic.Interaction
import Logic.DefaultActions
import Map.Room

import Text.LParse.Parser
import Text.LParse.Prebuilt

import Control.Applicative

lookAction :: Parser r String (GameAction ())
lookAction = do
    consume "look" 
    l <- (lookAt <$> (some (consumeSingle ' ') >> full)) <|> return look
    eoi 
    return l 

action :: Parser r String (GameAction ())
action = lookAction
    <|> (consume "inventory" >> return viewInv << eoi)
    <|> (go <$> (consume "go " >> word << eoi))