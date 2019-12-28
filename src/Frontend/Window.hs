module Frontend.Window where

import Frontend.Canvas
import Frontend.State
import Frontend.TAIO
import Frontend.Text

import Logic.StateStack

import Control.Lens hiding (view)
import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import qualified Data.Map.Strict as M
import Graphics.Gloss

renderWindow :: StateStack -> Window -> TAIO ()
renderWindow ss win = do
    (cw,ch) <- use (settings.dimensions)
    x <- resolveLocation X (win^.left)
    y <-  resolveLocation Y (win^.top)
    x' <- resolveLocation X (win^.right) 
    y' <- resolveLocation Y (win^.bottom)
    let w = x'-x + 1
    let h = y'-y + 1
    let v = win^.view
    let hnd = win^.handle
    fs <- get
    forM_ [x+1..x'-1] $ \px -> do
        (canvas.grid.at (px,y)) ?= CanvasCell (CharCell '-') []
        (canvas.grid.at (px,y')) ?= CanvasCell (CharCell '-') []
    forM_ [y+1..y'-1] $ \py -> do
        (canvas.grid.at (x,py)) ?= CanvasCell (CharCell '|') []
        (canvas.grid.at (x',py)) ?= CanvasCell (CharCell '|') []
    (canvas.grid.at (x,y)) ?= CanvasCell (CharCell '+') []
    (canvas.grid.at (x',y)) ?= CanvasCell (CharCell '+') []
    (canvas.grid.at (x,y')) ?= CanvasCell (CharCell '+') []
    (canvas.grid.at (x',y')) ?= CanvasCell (CharCell '+') []
    sequence_ $ concat $ zipWith (\py -> zipWith (\px c -> canvas.grid.at (px,py) ?= CanvasCell (CharCell c) []) [x+1..x'-1])  [y+1..y'-1] $ v ss fs
    when (hnd == 0) $ do
        let cy = fromIntegral $ length (v ss fs)
        let cx = fromIntegral $ length (last (v ss fs))
        (canvas.grid.at (x+cx+1,y+cy)) ?= CanvasCell CursorCell []