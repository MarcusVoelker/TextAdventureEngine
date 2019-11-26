module Logic.StateStack where

import Control.Lens
import Logic.Dialogue

data StackedState = DialogueState {
    _stackedStateDialogue :: DialogueTree
} | MenuState

makeFields ''StackedState