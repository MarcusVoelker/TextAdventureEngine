module Parser.RoomParser where

import Parser.Tokenizer hiding (Keyword(Room))
import qualified Parser.Tokenizer as T (Keyword(Room))

import Map.Room

import Control.DoubleContinuations
import Text.LParse.Parser
import Text.LParse.Prebuilt

import Control.Applicative
import Control.Monad
import Control.Lens.Getter
import Data.List
import Data.Maybe
import qualified Data.Map.Strict as M

data Property = SProp String | ListProp [Property] | PairProp (Property,Property) deriving Eq

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
    consumeSingle $ Op Colon
    c <- property
    consumeSingle Separator
    return (p,c)

room :: M.Map String Room -> Parser r [Token] Room
room rs = do
    consumeSingle $ Keyword T.Room
    (Identifier idt) <- tokenReturn
    consumeSingle Separator
    consumeSingle BlockStart
    ps <- many propertyLine
    consumeSingle BlockEnd
    (SProp name) <- return $ fromMaybe (SProp idt) (lookup "name" ps)
    (SProp description) <- return $ fromMaybe (SProp "") (lookup "description" ps)
    (ListProp objects) <- return $ fromMaybe (ListProp []) (lookup "objects" ps)
    (ListProp exits) <- return $ fromMaybe (ListProp []) (lookup "exits" ps)
    let exs = mapMaybe (\(PairProp (SProp d,SProp n)) -> (d,) <$> M.lookup n rs) exits
    return $ Room 
        idt
        name
        (const description)
        (const (\s ->
            fromMaybe ("I don't see any " ++ s) $ listToMaybe $ mapMaybe ((>>=(\case 
                (ListProp os,SProp d) | SProp s `elem` os -> Just d
                _ -> Nothing
                )) 
                .(\case
                (PairProp p) -> Just p 
                _ -> Nothing
            )) objects 
        ))
        (defaultGetExit (\_ n -> lookup n exs))

rooms :: Parser r [Token] (M.Map String Room)
rooms = pfix $ \rs -> do
    list <- many (room rs)
    return $ M.fromList $ map (\r -> (r^.idt,r)) list