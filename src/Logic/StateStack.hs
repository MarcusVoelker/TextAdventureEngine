module Logic.StateStack (
    StateStack,
    MenuState(..),
    StackedState,
    noContext,
    openMenuContext,
    closeContext,
    contextCount,
    menuState,
    applyMenuFunction,
    applyGameFunction,
    applyMenuFunctionM,
    applyGameFunctionM,
    stateAt,
    globalGameState,
    buildStateStack,
    SStepper,
    GameStepper,
    MenuStepper,
    Stepper(..),
    StateResult(..),
    push,
    pop,
    step0,
    step1,
    step2,
    stepStateStack
    ) where

import Logic.GameState
import Logic.Menu

import Control.Lens
import Control.Monad.Fail

import Data.Maybe

import Prelude hiding (fail)

data StackedState = SSMenu MenuState | SSGame GameState

data MenuState = MenuState {
    _stackedStateMenu :: Menu,
    _stackedStateIndex :: Int
}

newtype StateStack = StateStack {
    _stateStackStack :: [StackedState]
}

makeFields ''MenuState
makeFields ''StateStack

stackTop :: Lens' StateStack StackedState
stackTop = lens
    (\s -> head (s^.stack))
    (\s ss -> over stack ((ss:).tail) s)

stateAt :: Int -> StateStack -> Maybe StackedState
stateAt n ss = stateAt' n (reverse (ss^.stack))
    where
        stateAt' _ [] = Nothing
        stateAt' 0 (x:_) = Just x
        stateAt' n (_:xs) = stateAt' (n-1) xs

globalGameState :: Lens' StateStack GameState
globalGameState = lens
    (\s -> (\(SSGame gs) -> gs) $ last (s^.stack))
    (\s ss -> over stack ((++[SSGame ss]).init) s)

contextCount :: StateStack -> Int
contextCount ss = length $ ss^.stack

noContext :: StateStack -> Bool
noContext = (==1) . contextCount

openContext :: StackedState -> StateStack -> StateStack
openContext ns ss = ss&stack %~ (ns:)

openMenuContext :: MenuState -> StateStack -> StateStack
openMenuContext = openContext . SSMenu

closeContext :: StateStack -> StateStack
closeContext ss = ss&stack %~ tail

buildStateStack :: GameState -> StateStack
buildStateStack gs = StateStack [SSGame gs]

applyMenuFunction :: a -> (MenuState -> a) -> StackedState -> a
applyMenuFunction _ f (SSMenu ss) = f ss
applyMenuFunction def _ _ = def

applyGameFunction :: a -> (GameState -> a) -> StackedState -> a
applyGameFunction _ f (SSGame ss) = f ss
applyGameFunction def _ _ = def

applyMenuFunctionM :: (MonadFail m) => (MenuState -> m a) -> StackedState -> m a
applyMenuFunctionM f (SSMenu ss) = f ss
applyMenuFunctionM _ _ = fail "Expected Menu State!"

applyGameFunctionM :: (MonadFail m) => (GameState -> m a) -> StackedState -> m a
applyGameFunctionM f (SSGame ss) = f ss
applyGameFunctionM _ _ = fail "Expected Game State!"

data StateResult a = Pop | Change a | Push a [StackedState]

push :: StackedState -> StateResult a -> StateResult a
push ss (Change a) = Push a [ss]
push ss (Push a sss) = Push a (ss:sss)

pop :: StateResult a -> StateResult a
pop (Change a) = Pop
pop (Push a [ss]) = Change a
pop (Push a (ss:sss)) = Push a sss

type SStepper m a = a -> m (StateResult a)

type GameStepper m = SStepper m GameState
type MenuStepper m = SStepper m MenuState

data Stepper m = Stepper {
    _stepperGameStepper :: GameStepper m,
    _stepperMenuStepper :: MenuStepper m
}

makeFields ''Stepper

step0 :: (Monad m) => SStepper m a
step0 _ = return Pop

step1 :: (Monad m) => (a -> m a) -> SStepper m a
step1 f gs = f gs >>= (\gs -> return (Change gs))

step2 :: (Monad m) => (a -> m (a,StackedState)) -> SStepper m a
step2 f gs = f gs >>= (\(gs,ss) -> return (Push gs [ss]))

stepStateStack :: (Monad m) => Stepper m -> StateStack -> m StateStack
stepStateStack stepper (StateStack (SSMenu x:xs)) = (stepper^.menuStepper) x >>= (\case 
    Pop -> return $ StateStack xs
    Change ms -> return $ StateStack (SSMenu ms:xs)
    Push ms ns -> return $ StateStack (ns++(SSMenu ms:xs))
    )
stepStateStack stepper (StateStack (SSGame x:xs)) = (stepper^.gameStepper) x >>= (\case 
    Pop -> return $ StateStack xs
    Change gs -> return $ StateStack (SSGame gs:xs)
    Push gs ns -> return $ StateStack (ns++(SSGame gs:xs))
    )
stepStateStack _ (StateStack []) = return (StateStack [])

menuState :: Menu -> StackedState
menuState m = SSMenu (MenuState m 0)