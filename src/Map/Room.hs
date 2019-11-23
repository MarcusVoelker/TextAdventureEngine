module Map.Room where

import Thing

import Control.Lens

data Room = Room {
    _roomIdt :: String,
    _roomName :: String,
    _roomDescription :: String,
    _roomGetExit :: String -> Either String Room
}

makeFields ''Room

instance Eq Room where
    a == b = a^.idt == b^.idt

instance Ord Room where
    compare a b = compare (a^.idt) (b^.idt)

defaultGetExit :: (String -> Maybe Room) -> String -> Either String Room
defaultGetExit ge e = 
    case ge e of 
        Nothing -> Left $ if e `elem` map return "nsweud" then "I see no way to go " ++ e ++ "." else "I don't know '" ++ e ++ "'."
        Just r' -> Right r'