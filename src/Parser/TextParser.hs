module Parser.TextParser (variadicText) where

import GameData.Text

import Text.LParse.Parser
import Text.LParse.Prebuilt

import Control.Applicative

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
rawText = RawText <$> some (nParse (/= '\\') tokenReturn "Not Raw")

variadicText :: Parser t String VariadicText
variadicText = many (conditionalText <|> rawText)