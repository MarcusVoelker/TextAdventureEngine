module Map.Room where

import Thing

import Control.Lens

data Room s = Room {
    _roomIdt :: String,
    _roomName :: String,
    _roomDescription :: String,
    _roomGetExit :: String -> Either String (Room s)
}

makeFields ''Room

instance Eq (Room s) where
    a == b = a^.idt == b^.idt

instance Ord (Room s) where
    compare a b = compare (a^.idt) (b^.idt)

defaultGetExit :: (String -> Maybe (Room s)) -> String -> Either String (Room s)
defaultGetExit ge e = 
    case ge e of 
        Nothing -> Left $ if e `elem` map return "nsweud" then "I see no way to go " ++ e ++ "." else "I don't know '" ++ e ++ "'."
        Just r' -> Right r'