module Frontend.Canvas where

import Control.Lens
import qualified Data.Map.Strict as M

data CellContent = BlankCell | CharCell Char | CursorCell deriving (Show,Eq)

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
