module Parser.ObjectParser where

import Parser.Tokenizer

import Control.DoubleContinuations
import Text.LParse.Parser
import Text.LParse.Prebuilt

import Control.Applicative

data Property = TagProp | SProp String | ListProp [Property] | PairProp (Property,Property) deriving (Eq,Show)

data Object = Object {
    objType :: Keyword,
    id :: String,
    properties :: [(String,Property)]
}


isSProp :: Property -> Bool
isSProp (SProp _) = True
isSProp _ = False

property :: Parser r [Token] Property
property = (do
    (SLit s) <- tokenReturn
    return $ SProp s
    ) <|> (do
        consumeSingle $ Op LBrack
        ps <- sepMany (consumeSingle (Op Comma)) property
        consumeSingle $ Op RBrack
        return $ ListProp ps
    ) <|> (do
        consumeSingle $ Op LParen
        f <- property
        consumeSingle $ Op Comma
        s <- property
        consumeSingle $ Op RParen
        return $ PairProp (f,s) 
    ) 

propertyLine :: Parser r [Token] (String,Property)
propertyLine = do
    (Identifier p)  <- tokenReturn
    (do 
        consumeSingle $ Op Colon
        c <- property
        consumeSingle Separator
        return (p,c)
        ) <|> consumeSReturn Separator (p,TagProp)

object :: Parser r [Token] Object
object = do
    (Keyword kw) <- tokenReturn
    (Identifier idt) <- tokenReturn
    consumeSingle Separator
    consumeSingle BlockStart
    ps <- many propertyLine
    consumeSingle BlockEnd
    return $ Object kw idt ps