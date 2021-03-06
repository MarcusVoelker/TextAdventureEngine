module Frontend.CanvasRenderer where

import Frontend.Canvas
import Frontend.Primitives
import Frontend.State
import Frontend.FrontState
import Frontend.Text

import Control.Lens
import Control.Monad
import qualified Data.Map.Strict as M
import Data.Maybe
import Graphics.Gloss

import Thing

renderContent :: CellContent -> FrontRead Picture
renderContent BlankCell = return Blank
renderContent (CharCell c) = return (renderChar c)
renderContent CursorCell = do
  et <- view elapsedTime
  return $ if mod (floor et) 2 == 1 then rect (0,0) (7,15) else Blank
renderContent (EffectCell Shake c) = do
    et <- view elapsedTime
    Translate 0 (fromIntegral (mod (floor (et*30)) 5)-2) <$> renderContent c
renderContent (EffectCell (Coloured r g b) c) = Color (makeColor (fromIntegral r/255) (fromIntegral g/255) (fromIntegral b/255) 1) <$> renderContent c

renderCell :: (Int,Int) -> CanvasCell -> FrontRead Picture
renderCell (x,y) cell = do
    (fw,fh) <- views (settings.fontDimensions) (bimap fromIntegral fromIntegral)
    pic <- renderContent (cell^.content)
    return $ Translate (fromIntegral x*fw) (-fromIntegral y*fh) $ Color green pic

renderCanvas :: FrontRead Picture
renderCanvas = do
    (cw,ch) <- views (settings.dimensions) (bimap fromIntegral fromIntegral)
    (fw,fh) <- views (settings.fontDimensions) (bimap fromIntegral fromIntegral)
    g <- view (canvas.grid)
    Translate (fw*(-cw/2)+1) (fh*(ch/2-1)) . Pictures <$> sequence (M.foldrWithKey (\pos cell list -> renderCell pos cell : list) [] g)

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
    c <- use (canvas.grid.at pos)
    when (isJust c && (fromJust c^.content /= BlankCell)) $ writeToCanvas pos BlankCell

clearCanvas :: FrontMod () 
clearCanvas = (canvas.grid) .= M.empty