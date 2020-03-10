module Parser.TextParser (metaText) where

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

conditionalText :: Parser r String MetaLexeme
conditionalText = do
    consume "\\if{"
    c <- condition
    consume "}{"
    lex <- metaText
    consume "}"
    return $ ConditionalText c lex

rawText :: Parser r String MetaLexeme
rawText = RawText <$> some (nParse (`notElem` "\\{}") tokenReturn "Not Raw")

renderDirection :: Parser r String RenderDirection
renderDirection = 
    consumeReturn "shake" Shake
    <|> (consume "color" >> 
        (Color <$> 
            (fromIntegral <$> (consumeSingle ' ' >> integer)) 
            <*> (fromIntegral <$> (consumeSingle ' ' >> integer)) 
            <*> (fromIntegral <$> (consumeSingle ' ' >> integer))))

renderText :: Parser r String MetaLexeme
renderText = do
    consume "\\render{"
    r <- renderDirection
    consume "}{"
    lex <- metaText
    consume "}"
    return $ MRenderText r lex

metaText :: Parser t String MetaText
metaText = many (conditionalText <|> renderText <|> rawText)