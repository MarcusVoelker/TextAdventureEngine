module Frontend.State where

import Frontend.Canvas

import Logic.StateStack

import Control.Lens
import qualified Data.Map as M

type WHandle = Int

type View = StateStack -> FrontendState -> [String]

data ScreenLoc = Absolute Int | Relative Float
data LocKind = X | Y

data Window = Window {
    _windowHandle :: WHandle,
    _windowLeft :: ScreenLoc,
    _windowTop :: ScreenLoc,
    _windowRight :: ScreenLoc,
    _windowBottom :: ScreenLoc,
    _windowView :: View,
    _windowContext :: Int
}

data FrontendSettings = FrontendSettings {
    _frontendSettingsDimensions :: (Int,Int),
    _frontendSettingsFontDimensions :: (Int,Int)
}

data InputState = InputState {
    _inputStateBuffer :: String,
    _inputStateHistory :: [String],
    _inputStateHistoryPointer :: Int,
    _inputStateSearch :: Maybe String
}

data FrontendState = FrontendState {
    _frontendStateTextHistory :: [String],
    _frontendStateInput :: InputState,
    _frontendStateCanvas :: CanvasState,
    _frontendStateWindows :: M.Map WHandle Window,
    _frontendStateSettings :: FrontendSettings,
    _frontendStateElapsedTime :: Float
}

makeFields ''InputState

makeFields ''FrontendState

makeFields ''FrontendSettings

makeFields ''Window