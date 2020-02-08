module Parser.TextParser (variadicText) where

import GameData.Text

import Text.LParse.Parser
import Text.LParse.Prebuilt

import Control.Applicative
import Data.List

true :: Parser r String VarCondition
true = consume "true" >> return CTrue

isTrue :: Parser r String VarCondition
isTrue = consume "isTrue " >> (CIsTrue <$> word)

condition :: Parser r String VarCondition
condition = true <|> isTrue

conditionalText :: Parser r String VariadicLexeme
conditionalText = do
    consume "\\if{"
    c <- condition
    consume "}{"
    lex <- variadicText
    consume "}"
    return $ ConditionalText c lex

rawText :: Parser r String VariadicLexeme
rawText = RawText <$> some (nParse (`notElem` "\\{}") tokenReturn "Not Raw")

renderDirection :: Parser r String RenderDirection
renderDirection = 
    consumeReturn "shake" Shake
    <|> (consume "color" >> 
        (Color <$> 
            (fromIntegral <$> (consumeSingle ' ' >> integer)) 
            <*> (fromIntegral <$> (consumeSingle ' ' >> integer)) 
            <*> (fromIntegral <$> (consumeSingle ' ' >> integer))))

renderText :: Parser r String VariadicLexeme
renderText = do
    consume "\\render{"
    r <- renderDirection
    consume "}{"
    lex <- variadicText
    consume "}"
    return $ VRenderText r lex

variadicText :: Parser t String VariadicText
variadicText = many (conditionalText <|> renderText <|> rawText)