module Parser.ActionParser where

import Actions.GameState
import Actions.Interaction
import Actions.DefaultActions
import Map.Room

import Text.LParse.Parser
import Text.LParse.Prebuilt

import Control.Applicative

action :: Parser r String (GameAction Room ())
action = (consume "look" >> eoi >> return look)
    <|> (go <$> ((consume "go ") >> word << eoi))