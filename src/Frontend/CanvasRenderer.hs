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
import Graphics.UI.GLUT hiding (elapsedTime, rect)

import Thing

type Rendering = IO ()

renderContent :: CellContent -> FrontRead Rendering
renderContent BlankCell = return $ return ()
renderContent (CharCell c) = return (renderChar c)
renderContent CursorCell = do
  et <- view elapsedTime
  return $ if mod (floor et) 2 == 1 then rect (0,0) (7,15) else return ()
renderContent (EffectCell Shake c) = do
    et <- view elapsedTime
    content <- renderContent c
    return $ preservingMatrix $ do
        translate $ Vector3 (0 :: GLdouble) (fromIntegral (mod (floor (et*30)) 5)-2) 0
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
    lift $ do
        loadIdentity
        ortho 0 (cw*fw) 0 (ch*fh) (-5) (5)
        clear [ColorBuffer]
    g <- view (canvas.grid)
    sequence_ <$> sequence (M.foldrWithKey (\pos cell list -> renderCell pos cell : list) [] g)

writeToCanvas :: (Int,Int) -> CellContent -> FrontMod ()
writeToCanvas pos c = do
    curC <- use (canvas.grid.at pos)
    et <- use elapsedTime
    (canvas.grid.at pos) ?= CanvasCell c (curC >>= (\curCC ->
        if (curCC^.content /= BlankCell) && (curCC^.content /= CursorCell) && c == BlankCell then
            Just (et+0.1,curCC^.content)
        else
            curCC^.fadeout
        ))

clearCell :: (Int,Int) -> FrontMod () 
clearCell pos = do
    c <- use (fusing (canvas.grid.at pos))
    when (isJust c && (fromJust c^.content /= BlankCell)) $ writeToCanvas pos BlankCell

clearCanvas :: FrontMod () 
clearCanvas = (canvas.grid) .= M.empty