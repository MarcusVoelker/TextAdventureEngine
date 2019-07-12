module Parser.Tokenizer where

import Control.Applicative
import Control.Monad
import Data.Maybe

import Text.LParse.Parser
import Text.LParse.Prebuilt

data Token = Keyword Keyword | SLit String | Identifier String | Op Operator | Indent Int | Whitespace deriving (Show,Eq)

data TokenTree = TokenTree [Token] [TokenTree]

instance Show TokenTree where
    show = showTT 0

showTT :: Int -> TokenTree -> String
showTT i (TokenTree l tts) = ([1..i] >> " ") ++ show l ++ "\n" ++ concatMap (showTT (i+2)) tts

data Keyword = Room deriving (Show,Eq)

data Operator = Colon deriving (Show,Eq)

keyword :: Parser r String Token
keyword = consumeReturn "Room" (Keyword Room)

slit :: Parser r String Token
slit = SLit <$> surround "\"\"" (many (nParse (/= '"') tokenReturn "Internal Error"))

ident :: Parser r String Token
ident = Identifier <$> ((:) <$> letter <*> many (letter <|> ((head . show) <$> digit)))

op :: Parser r String Token
op = consumeSReturn ':' $ Op Colon

whitespace :: Parser r String Token
whitespace = some (consumeSingle ' ' <|> consumeSingle '\t') >> return Whitespace

newline :: Parser r String ()
newline = consumeSingle '\n' <|> consume "\r\n"

indent :: Parser r String Token
indent = (Indent . length) <$> many (consumeSingle ' ' <|> consumeSingle '\t')

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
tokenizer = (>>= id) <$> (many tokenizeLine << eoi)

blocker' :: Int -> Parser r [Token] TokenTree
blocker' i = TokenTree <$> many (nParse (not . isIndent) tokenReturn "Internal Error") <*> many (nParse (\t -> case t of Indent i' | i' > i -> True; _ -> False) (((\(Indent i') -> i') <$> tokenReturn) >>= blocker') "Internal Error")

blocker :: Parser r [Token] TokenTree
blocker = removeEmpty <$> (TokenTree <$> return [] <*> many (consumeSingle (Indent 0) >> blocker' 0)) << eoi

removeEmpty' :: TokenTree -> Maybe TokenTree
removeEmpty' (TokenTree [] []) = Nothing
removeEmpty' (TokenTree [] xs) = (\xs' -> if null xs' then Nothing else Just (TokenTree [] xs')) (mapMaybe removeEmpty' xs)
removeEmpty' (TokenTree l xs) = Just (TokenTree l $ mapMaybe removeEmpty' xs)

removeEmpty :: TokenTree -> TokenTree
removeEmpty = fromJust . removeEmpty'