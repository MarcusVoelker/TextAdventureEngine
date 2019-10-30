module Map.Room where

import Logic.GameState

data Room = Room {
    idt :: String,
    name :: String,
    description :: GameState Room -> String,
    objDescription :: GameState Room -> String -> String,
    getExit :: GameState Room -> String -> Either String Room
}

defaultGetExit :: (GameState Room -> String -> Maybe Room) -> GameState Room -> String -> Either String Room
defaultGetExit ge s e = 
    case ge s e of 
        Nothing -> Left $ if e `elem` map return "nsweud" then "I see no way to go " ++ e ++ "." else "I don't know '" ++ e ++ "'."
        Just r' -> Right r'