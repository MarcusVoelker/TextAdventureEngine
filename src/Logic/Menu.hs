module Logic.Menu where

import Control.Lens

import GameData.Text
import Logic.DefaultActions
import Logic.Interaction

import Thing

data MenuEntryContent = MenuAction {
    _menuEntryContentAction :: GameAction ()
} | MenuBack

data MenuEntry = MenuEntry {
    _menuEntryName :: MetaText,
    _menuEntryContent :: MenuEntryContent
}

data Menu = Menu {
    _menuContent :: [MenuEntry]
}

makeFields ''MenuEntryContent
makeFields ''MenuEntry
makeFields ''Menu

mainMenu :: Menu
mainMenu = Menu [
    MenuEntry [RawText "Start"] MenuBack,
    MenuEntry [RawText "Save"] (MenuAction save),
    MenuEntry [RawText "Load"] (MenuAction load),
    MenuEntry [RawText "Quit"] (MenuAction quit)
    ]