module Parser.Tokenizer where

import Control.Applicative
import Control.Monad
import Data.Maybe

import Text.LParse.Parser
import Text.LParse.Prebuilt

data Token = Keyword Keyword | SLit String | Identifier String | Op Operator | Indent Int | Whitespace | Separator | BlockStart | BlockEnd deriving (Show,Eq)

data Keyword = Room | Entity deriving (Show,Eq)

data Operator = Colon | LBrack | RBrack | LParen | RParen | Comma deriving (Show,Eq)

keyword :: Parser r String Token
keyword = consumeReturn "Room" (Keyword Room)
    <|> consumeReturn "Entity" (Keyword Entity)

slit :: Parser r String Token
slit = SLit <$> surround "\"\"" (many (nParse (/= '"') tokenReturn "Internal Error"))

ident :: Parser r String Token
ident = Identifier <$> ((:) <$> letter <*> many (letter <|> (head . show <$> digit)))

op :: Parser r String Token
op = consumeSReturn ':' (Op Colon)
    <|> consumeSReturn '[' (Op LBrack)
    <|> consumeSReturn ']' (Op RBrack)
    <|> consumeSReturn '(' (Op LParen)
    <|> consumeSReturn ')' (Op RParen)
    <|> consumeSReturn ',' (Op Comma)
    <|> consumeSReturn ';' Separator

whitespace :: Parser r String Token
whitespace = some (consumeSingle ' ' <|> consumeSingle '\t') >> return Whitespace

newline :: Parser r String ()
newline = consumeSingle '\n' <|> consume "\r\n"

indent :: Parser r String Token
indent = Indent . length <$> many (consumeSingle ' ' <|> consumeSingle '\t')

isW :: Token -> Bool
isW Whitespace = True
isW _ = False

isIndent :: Token -> Bool
isIndent (Indent _) = True
isIndent _ = False

tokenizeLineContent :: Parser r String [Token]
tokenizeLineContent = filter (not . isW) <$> many (keyword <|> slit <|> ident <|> op <|> whitespace) 

tokenizeLine :: Parser r String [Token]
tokenizeLine = ((:) <$> indent <*> tokenizeLineContent) << newline

tokenizer :: Parser r String [Token]
tokenizer = (++[Indent 0]) . (>>= id) <$> (many tokenizeLine << eoi)

blocker' :: Int -> Parser r [Token] [Token]
blocker' i = do
    line <- many (nParse (not . isIndent) tokenReturn "Internal Error")
    i' <- (\(Indent i') -> i') <$> tokenReturn
    if i' > i then 
        (\b -> line ++ (Separator : BlockStart : b) ++ [BlockEnd]) <$> blocker' i'
    else if i' == i then
        ((line ++ [Separator]) ++) <$> blocker' i
    else
        return (line ++ [Separator])

blocker :: Parser r [Token] [Token]
blocker = (>>= id) <$> (consumeSingle (Indent 0) >> many (blocker' 0) << eoi)