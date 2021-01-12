module Frontend.CanvasRenderer where

import Frontend.Canvas
import Frontend.Primitives
import Frontend.State
import Frontend.FrontState
import Frontend.Text

import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class
import qualified Data.Map.Strict as M
import Data.Maybe
import Graphics.UI.GLUT hiding (rect)

import Thing

type Rendering = IO ()

renderContent :: CellContent -> FrontRead Rendering
renderContent BlankCell = return $ return ()
renderContent (CharCell c) = return $ do
    color $ Color3 (0 :: GLfloat) 1 0
    renderChar c
renderContent CursorCell = do
  et <- get elapsedTime
  return $ if mod et 2000 < 1000 then rect (0,0) (7,15) else return ()
renderContent (EffectCell Shake c) = do
    et <- get elapsedTime
    content <- renderContent c
    return $ preservingMatrix $ do
        translate $ Vector3 (0 :: GLdouble) (fromIntegral (mod (div (et*3) 100) 5)-2) 0
        content
renderContent (EffectCell (Coloured r g b) c) = do
    content <- renderContent c
    return $ do
        color $ Color3 (fromIntegral r/255 :: GLdouble) (fromIntegral g/255) (fromIntegral b/255)
        content

renderCell :: (Int,Int) -> CanvasCell -> FrontRead Rendering
renderCell (x,y) cell = do
    (cw,ch) <- views (settings.dimensions) (bimap fromIntegral fromIntegral)
    (fw,fh) <- views (settings.fontDimensions) (bimap fromIntegral fromIntegral)
    content <- renderContent (cell^.content)
    return $ preservingMatrix $ do 
        translate $ Vector3 (fromIntegral x*fw) ((ch-1-fromIntegral y)*fh) (0 :: GLdouble)
        content

renderCanvas :: FrontRead Rendering
renderCanvas = do
    (cw,ch) <- views (settings.dimensions) (bimap fromIntegral fromIntegral)
    (fw,fh) <- views (settings.fontDimensions) (bimap fromIntegral fromIntegral)
    g <- view (canvas.grid)
    sequence_ <$> sequence (M.foldrWithKey (\pos cell list -> renderCell pos cell : list) [] g)

writeToCanvas :: (Int,Int) -> CellContent -> FrontMod ()
writeToCanvas pos c = do
    curC <- use (canvas.grid.at pos)
    et <- get elapsedTime
    (canvas.grid.at pos) ?= CanvasCell c (curC >>= (\curCC ->
        if (curCC^.content /= BlankCell) && (curCC^.content /= CursorCell) && c == BlankCell then
            Just (et+100,curCC^.content)
        else
            curCC^.fadeout
        ))

clearCell :: (Int,Int) -> FrontMod () 
clearCell pos = do
    c <- use (fusing (canvas.grid.at pos))
    when (isJust c && (fromJust c^.content /= BlankCell)) $ writeToCanvas pos BlankCell

clearCanvas :: FrontMod () 
clearCanvas = (canvas.grid) .= M.empty