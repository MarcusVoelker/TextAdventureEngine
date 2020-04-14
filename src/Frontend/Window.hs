module Frontend.Window where

import Frontend.Canvas
import Frontend.CanvasRenderer
import Frontend.FrontState
import Frontend.State
import Frontend.Text

import GameData.Text

import Logic.StateStack

import Control.Lens
import Control.Monad
import Control.Monad.Trans.State

singleStyle :: WindowStyle
singleStyle = WindowStyle
    bSHLine
    bSVLine
    bSTLCorner
    bSTRCorner
    bSBLCorner
    bSBRCorner

doubleStyle :: WindowStyle
doubleStyle = WindowStyle
    bDHLine
    bDVLine
    bDTLCorner
    bDTRCorner
    bDBLCorner
    bDBRCorner

renderWindow :: StateStack -> Window -> FrontMod ()
renderWindow ss win = do
    x <- liftRead $ resolveLocation X (win^.left)
    y <-  liftRead $ resolveLocation Y (win^.top)
    x' <- liftRead $ resolveLocation X (win^.right) 
    y' <- liftRead $ resolveLocation Y (win^.bottom)
    let v = win^.contentView
    let hnd = win^.handle
    let sty = win^.style
    fs <- get
    forM_ [x+1..x'-1] $ \px -> do
        writeToCanvas (px,y) (CharCell $ sty^.hStyle)
        writeToCanvas (px,y') (CharCell $ sty^.hStyle)
    forM_ [y+1..y'-1] $ \py -> do
        writeToCanvas (x,py) (CharCell $ sty^.vStyle)
        writeToCanvas (x',py) (CharCell $ sty^.vStyle)
    writeToCanvas (x,y) (CharCell $ sty^.cTLStyle)
    writeToCanvas (x',y) (CharCell $ sty^.cTRStyle)
    writeToCanvas (x,y') (CharCell $ sty^.cBLStyle)
    writeToCanvas (x',y') (CharCell $ sty^.cBRStyle)
    forM_ [(px,py) | px <- [x+1..x'-1], py <- [y+1..y'-1]] clearCell
    let code = case stateAt (win^.context) ss of Nothing -> [["Unknown Context" :: ResolvedLexeme],[Text $ show (win^.context)]]; Just st -> v st fs
    sequence_ $ concat $ zipWith (\py -> zipWith (\px -> writeToCanvas (px,py)) [x+1..x'-1]) [y+1..y'-1] $ map cellize code
    when (hnd == 0) $ do
        let cy = length code
        let cx = length (cellize $ last code)
        writeToCanvas (x+cx+1,y+cy) CursorCell