module Frontend.State where

import Frontend.Canvas

import Logic.StateStack
import Logic.Deserialiser
import GameData.Text

import Control.Lens
import qualified Data.Map.Strict as M

type WHandle = Int

type ContentView = StackedState -> FrontendState -> [ResolvedText]

data ScreenLoc = Absolute Int | Relative Float
data LocKind = X | Y

data WindowStyle = WindowStyle {
    _windowStyleHStyle :: Char,
    _windowStyleVStyle :: Char,
    _windowStyleCTLStyle :: Char,
    _windowStyleCTRStyle :: Char,
    _windowStyleCBLStyle :: Char,
    _windowStyleCBRStyle :: Char
}

data Window = Window {
    _windowHandle :: WHandle,
    _windowLeft :: ScreenLoc,
    _windowTop :: ScreenLoc,
    _windowRight :: ScreenLoc,
    _windowBottom :: ScreenLoc,
    _windowContentView :: ContentView,
    _windowContext :: Int,
    _windowStyle :: WindowStyle
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
    _frontendStateTextHistory :: [ResolvedText],
    _frontendStateInput :: InputState,
    _frontendStateCanvas :: CanvasState,
    _frontendStateWindows :: M.Map WHandle Window,
    _frontendStateSettings :: FrontendSettings,
    _frontendStateDeserialisationContext :: DeserialisationContext
}

makeFields ''InputState

makeFields ''FrontendState

makeFields ''FrontendSettings

makeFields ''WindowStyle

makeFields ''Window