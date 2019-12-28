module Frontend.CanvasRenderer where

import Frontend.Canvas
import Frontend.Primitives
import Frontend.State
import Frontend.TAIO
import Frontend.Text

import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class
import Data.Bifunctor
import qualified Data.Map as M
import Graphics.Gloss

renderContent :: CellContent -> TAIO Picture
renderContent BlankCell = return Blank
renderContent (CharCell c) = return (renderChar c)
renderContent CursorCell = do
  et <- use elapsedTime
  return $ if mod (floor (et*0.7)) 2 == 1 then rect (0,0) (7,15) else Blank

renderCell :: (Int,Int) -> CanvasCell -> TAIO Picture
renderCell (x,y) cell = do
    (fw,fh) <- uses (settings.fontDimensions) (bimap fromIntegral fromIntegral)
    pic <- renderContent (cell^.content)
    return $ Translate (fromIntegral x*fw) (-fromIntegral y*fh) $ Color green pic

renderCanvas :: TAIO Picture
renderCanvas = do
    (cw,ch) <- uses (settings.dimensions) (bimap fromIntegral fromIntegral)
    (fw,fh) <- uses (settings.fontDimensions) (bimap fromIntegral fromIntegral)
    g <- use (canvas.grid)
    Translate (fw*(-cw/2)+1) (fh*(ch/2-1)) . Pictures <$> sequence (M.foldrWithKey (\pos cell list -> renderCell pos cell : list) [] g)