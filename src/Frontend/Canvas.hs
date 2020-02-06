module Frontend.Canvas where

import GameData.Text

import Control.Lens
import qualified Data.Map.Strict as M

data CellEffect = Coloured Int Int Int | Shake deriving (Show,Eq)

data CellContent = BlankCell | CharCell Char | EffectCell CellEffect CellContent | CursorCell deriving (Show,Eq)

data CanvasCell = CanvasCell {
    _canvasCellContent :: CellContent,
    _canvasCellFadeout :: Maybe (Float,CellContent)
} deriving (Show)

newtype CanvasState = CanvasState {
    _canvasStateGrid :: M.Map (Int,Int) CanvasCell
}

makeFields ''CanvasCell

makeFields ''CanvasState

initialCanvasState :: CanvasState
initialCanvasState = CanvasState M.empty

cellizeLexeme :: ResolvedLexeme -> [CellContent]
cellizeLexeme (Text s) = map CharCell s
cellizeLexeme (RenderText "shake" rt) = map (EffectCell Shake) $ cellize rt

cellize :: ResolvedText -> [CellContent]
cellize = concatMap cellizeLexeme