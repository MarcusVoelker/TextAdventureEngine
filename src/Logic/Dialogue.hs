module Logic.Dialogue where

import Control.Lens
import qualified Data.Map.Strict as M

data DialogueTree = DialogueTree {
    _dialogueTreeResponse :: String,
    _dialogueTreeNexts :: M.Map String (Maybe DialogueTree)
}

makeFields ''DialogueTree