module Map.Room where

import Control.Lens

data Room s = Room {
    _idt :: String,
    _name :: String,
    _description :: s -> String,
    _objDescription :: s -> String -> String,
    _getExit :: s -> String -> Either String (Room s)
}

makeLenses ''Room

instance Eq (Room s) where
    a == b = _idt a == _idt b

instance Ord (Room s) where
    compare a b = compare (_idt a) (_idt b)

defaultGetExit :: (s -> String -> Maybe (Room s)) -> s -> String -> Either String (Room s)
defaultGetExit ge s e = 
    case ge s e of 
        Nothing -> Left $ if e `elem` map return "nsweud" then "I see no way to go " ++ e ++ "." else "I don't know '" ++ e ++ "'."
        Just r' -> Right r'