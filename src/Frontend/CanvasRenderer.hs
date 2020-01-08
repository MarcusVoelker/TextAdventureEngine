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

renderContent :: CellContent -> FrontRead Picture
renderContent BlankCell = return Blank
renderContent (CharCell c) = return (renderChar c)
renderContent CursorCell = do
  et <- view elapsedTime
  return $ if mod (floor (et*0.7)) 2 == 1 then rect (0,0) (7,15) else Blank

renderCell :: (Int,Int) -> CanvasCell -> FrontRead Picture
renderCell (x,y) cell = do
    (fw,fh) <- views (settings.fontDimensions) (bimap fromIntegral fromIntegral)
    pic <- renderContent (cell^.content)
    if isNothing (cell^.fadeout) then
        return $ Translate (fromIntegral x*fw) (-fromIntegral y*fh) $ Color green pic
    else do
        et <- view elapsedTime
        let (f,c) = fromJust (cell^.fadeout)
        fpic <- renderContent c
        return $ Translate (fromIntegral x*fw) (-fromIntegral y*fh) $ Pictures [Color (makeColor 0 1 0 (f-et)) fpic,Color green pic]

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