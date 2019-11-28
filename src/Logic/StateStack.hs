module Logic.StateStack where

import Logic.Dialogue
import Logic.GameState
import Logic.Interaction
import Logic.Response

import Control.Lens
import Control.Monad.Trans.State

data StackedState = DialogueState {
    _stackedStateDialogue :: DialogueTree
} | MenuState

data StateStack = StateStack {
    _stateStackBottom :: GameState,
    _stateStackStack :: [StackedState]
}

makeFields ''StackedState
makeFields ''StateStack

type Action a = StateT StateStack Responding a
type TempAction a = StateT StackedState Responding a

liftBottom :: GameAction a -> Action a
liftBottom = zoom bottom

liftTemporary :: TempAction a -> Action a
liftTemporary = zoom $ lens
    (\s -> head (s^.stack))
    (\s ss -> over stack ((ss:).tail) s)

noContext :: StateStack -> Bool
noContext ss = null $ ss^.stack

openContext :: StackedState -> StateStack -> StateStack
openContext ns ss = ss&stack %~ (ns:)