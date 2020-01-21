module Logic.Event where

import Logic.Dialogue
import Logic.Item

import Map.Room

import Control.Lens

data UseEvent = UnlockDoor {
    _useEventDirection :: String,
    _useEventItem :: Item,
    _useEventNewRoom :: Room,
    _useEventConfirmation :: Maybe String
} | DisplayText {
    _useEventText :: String
} | GenericUseEvent

newtype TalkEvent = TalkEvent {
    _talkEventDialogueTree :: DialogueTree
}

makeFields ''UseEvent
makeFields ''TalkEvent