module Logic.Event where

import Logic.Item

import GameData.Room

import Control.Lens

data UseEvent = UnlockDoor {
    _useEventDirection :: String,
    _useEventItem :: Item,
    _useEventNewRoom :: Room,
    _useEventConfirmation :: Maybe String
} | DisplayText {
    _useEventText :: String
} | GenericUseEvent

makeFields ''UseEvent