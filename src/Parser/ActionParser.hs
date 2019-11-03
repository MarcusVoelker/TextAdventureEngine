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

takeAction :: Parser r String (GameAction ())
takeAction = do
    consume "take" 
    l <- (takeItem <$> (some (consumeSingle ' ') >> full)) <|> return look
    eoi 
    return l 

action :: Parser r String (GameAction ())
action = lookAction
    <|> takeAction
    <|> (consume "inventory" >> return viewInv << eoi)
    <|> (go <$> (consume "go " >> word << eoi))