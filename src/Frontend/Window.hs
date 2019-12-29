module Frontend.Window where

import Frontend.Canvas
import Frontend.CanvasRenderer
import Frontend.FrontState
import Frontend.State
import Frontend.Text

import Logic.StateStack

import Control.Lens
import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import qualified Data.Map.Strict as M
import Graphics.Gloss

renderWindow :: StateStack -> Window -> FrontMod ()
renderWindow ss win = do
    (cw,ch) <- use (settings.dimensions)
    x <- liftRead $ resolveLocation X (win^.left)
    y <-  liftRead $ resolveLocation Y (win^.top)
    x' <- liftRead $ resolveLocation X (win^.right) 
    y' <- liftRead $ resolveLocation Y (win^.bottom)
    let w = x'-x + 1
    let h = y'-y + 1
    let v = win^.contentView
    let hnd = win^.handle
    fs <- get
    forM_ [x+1..x'-1] $ \px -> do
        writeToCanvas (px,y) (CharCell '-')
        writeToCanvas (px,y') (CharCell '-')
    forM_ [y+1..y'-1] $ \py -> do
        writeToCanvas (x,py) (CharCell '|')
        writeToCanvas (x',py) (CharCell '|')
    writeToCanvas (x,y) (CharCell '+')
    writeToCanvas (x',y) (CharCell '+')
    writeToCanvas (x,y') (CharCell '+')
    writeToCanvas (x',y') (CharCell '+')
    forM_ [(px,py) | px <- [x+1..x'-1], py <- [y+1..y'-1]] clearCell
    sequence_ $ concat $ zipWith (\py -> zipWith (\px -> writeToCanvas (px,py) . CharCell) [x+1..x'-1])  [y+1..y'-1] $ v ss fs
    when (hnd == 0) $ do
        let cy = length (v ss fs)
        let cx = length (last (v ss fs))
        writeToCanvas (x+cx+1,y+cy) CursorCell