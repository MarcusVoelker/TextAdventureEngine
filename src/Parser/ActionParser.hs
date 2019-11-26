module Parser.ActionParser where

import Logic.GameState
import Logic.Interaction
import Logic.DefaultActions
import Map.Room

import Text.LParse.Parser
import Text.LParse.Prebuilt

import Control.Applicative
import Control.Monad hiding (fail)
import Control.Monad.Fail
import Data.List

import Prelude hiding (fail)

lookAction :: Parser r String (GameAction ())
lookAction = do
    consume "look" 
    l <- (lookAt <$> (some (consumeSingle ' ') >> full)) <|> return look
    eoi 
    return l 

takeAction :: Parser r String (GameAction ())
takeAction = do
    consume "take" 
    l <- takeItem <$> (some (consumeSingle ' ') >> full)
    eoi 
    return l 

talkAction :: Parser r String (GameAction ())
talkAction = do
    consume "talk" 
    l <- talkTo <$> (some (consumeSingle ' ') >> full)
    eoi 
    return l 

useAction :: Parser r String (GameAction ())
useAction = do
    consume "use "
    words <- sepSome (void $ some $ consumeSingle ' ') word
    eoi 
    if "on" `elem` words then
        (\(n,_:m) -> return $ useOn (unwords n) (unwords m))$ break (=="on") words
    else
        fail "expected 'on'"

action :: Parser r String (GameAction ())
action = lookAction
    <|> takeAction
    <|> talkAction
    <|> useAction
    <|> (consume "inventory" >> return viewInv << eoi)
    <|> (go <$> (consume "go " >> full << eoi))