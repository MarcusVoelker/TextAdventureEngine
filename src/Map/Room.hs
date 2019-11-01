module Map.Room where

import Control.Lens
import Logic.GameState

data Room = Room {
    _idt :: String,
    _name :: String,
    _description :: GameState Room -> String,
    _objDescription :: GameState Room -> String -> String,
    _getExit :: GameState Room -> String -> Either String Room
}

makeLenses ''Room

instance Eq Room where
    a == b = _idt a == _idt b

instance Ord Room where
    compare a b = compare (_idt a) (_idt b)

defaultGetExit :: (GameState Room -> String -> Maybe Room) -> GameState Room -> String -> Either String Room
defaultGetExit ge s e = 
    case ge s e of 
        Nothing -> Left $ if e `elem` map return "nsweud" then "I see no way to go " ++ e ++ "." else "I don't know '" ++ e ++ "'."
        Just r' -> Right r'