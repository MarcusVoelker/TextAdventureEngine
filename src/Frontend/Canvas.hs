module Frontend.Canvas where

import Frontend.Text

import Control.Lens
import qualified Data.Map as M

data CellContent = BlankCell | CharCell Char | CursorCell

data CanvasCell = CanvasCell {
    _canvasCellContent :: CellContent,
    _canvasCellFadeout :: [(Float,CellContent)]
}

newtype CanvasState = CanvasState {
    _canvasStateGrid :: M.Map (Int,Int) CanvasCell
}

makeFields ''CanvasCell

makeFields ''CanvasState

initialCanvasState :: CanvasState
initialCanvasState = CanvasState M.empty
