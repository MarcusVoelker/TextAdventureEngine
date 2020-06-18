module Logic.Menu where

import Control.Lens

import GameData.Text
import Logic.Response

import Thing

newtype MenuEntryContent = MenuAction {
    _menuEntryContentAction :: [Response]
} 

data MenuEntry = MenuEntry {
    _menuEntryName :: MetaText,
    _menuEntryContent :: MenuEntryContent
}

newtype Menu = Menu {
    _menuContent :: [MenuEntry]
}

makeFields ''MenuEntryContent
makeFields ''MenuEntry
makeFields ''Menu

mainMenu :: Menu
mainMenu = Menu [
    MenuEntry [RawText "Continue"] (MenuAction [LeaveContextResponse]),
    MenuEntry [RawText "Save"] (MenuAction [SaveResponse]),
    MenuEntry [RawText "Load"] (MenuAction [LoadResponse]),
    MenuEntry [RawText "Quit"] (MenuAction [QuitResponse])
    ]