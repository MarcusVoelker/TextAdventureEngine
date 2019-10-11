module Parser.ActionParser where

import Actions.GameState
import Actions.Interaction
import Actions.DefaultActions
import Map.Room

import Text.LParse.Parser
import Text.LParse.Prebuilt

import Control.Applicative

lookAction :: Parser r String (GameAction Room ())
lookAction = 
    consume "look" >> 
        ((eoi >> return look) <|> (lookAt <$> (word << eoi))) 


action :: Parser r String (GameAction Room ())
action = lookAction  
    <|> (go <$> (consume "go " >> word << eoi))