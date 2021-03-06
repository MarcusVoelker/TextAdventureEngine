module Parser.ActionParser where

import Logic.Interaction
import Logic.DefaultActions

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

useOnAction :: Parser r String (GameAction ())
useOnAction = do
    consume "use "
    words <- sepSome (void $ some $ consumeSingle ' ') word
    eoi 
    if "on" `elem` words then
        (\(n,_:m) -> return $ useOn (unwords n) (unwords m)) $ break (=="on") words
    else
        fail "expected 'on'"

useEntityAction :: Parser r String (GameAction ())
useEntityAction = do
    consume "use"
    l <- useEntity <$> (some (consumeSingle ' ') >> full)
    eoi 
    return l 

goSynonym :: Parser r String (GameAction ())
goSynonym = do
    t0 <- tokenReturn
    eoi
    if t0 `elem` ("nsweud" :: String) then
        return $ go [t0]
    else
        fail "unknown go synonym"

saveLoadQuitAction :: Parser r String (GameAction())
saveLoadQuitAction = (consumeReturn "save" save <|> consumeReturn "load" load <|> consumeReturn "quit" quit) << eoi

action :: Parser r String (GameAction ())
action = lookAction
    <|> takeAction
    <|> useOnAction
    <|> useEntityAction
    <|> goSynonym
    <|> saveLoadQuitAction
    <|> (consume "inventory" >> return viewInv << eoi)
    <|> (go <$> (consume "go " >> full << eoi))