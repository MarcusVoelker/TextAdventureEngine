module Frontend.Canvas where

import GameData.Text hiding (RenderDirection(Shake))
import qualified GameData.Text as T (RenderDirection(Shake))

import Control.Lens
import qualified Data.Map.Strict as M

import Thing

data CellEffect = Coloured Int Int Int | Shake deriving (Show,Eq)

data CellContent = BlankCell | CharCell Char | EffectCell CellEffect CellContent | CursorCell deriving (Show,Eq)

data CanvasCell = CanvasCell {
    _canvasCellContent :: CellContent,
    _canvasCellFadeout :: Maybe (Int,CellContent)
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
cellizeLexeme (RenderText T.Shake rt) = map (EffectCell Shake) $ cellize rt
cellizeLexeme (RenderText (Color a b c) rt) = map (EffectCell (Coloured a b c)) $ cellize rt

cellize :: ResolvedText -> [CellContent]
cellize = concatMap cellizeLexeme