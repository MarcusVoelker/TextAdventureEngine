module Frontend.Window where

import Frontend.Frontend

import Logic.StateStack

import Control.Lens hiding (view)
import Control.Monad.Trans.State
import qualified Data.Map.Strict as M
import Graphics.Gloss

rect :: (Float,Float) -> (Float,Float) -> Picture
rect (x,y) (w,h) = Polygon [(x,y),(x+w,y),(x+w,y+h),(x,y+h)]

hline :: (Float,Float) -> Float -> Picture
hline (x,y) l = rect (x,y) (l,1)

vline :: (Float,Float) -> Float -> Picture
vline (x,y) l = rect (x,y) (1,l)

pix :: (Float,Float) -> Picture
pix (x,y) = rect (x,y) (1,1)

chars :: M.Map Char Picture
chars = M.fromList [
    ('A',Pictures [
    hline (0,10) 7,
    hline (0,14) 7,
    vline (6,4) 11,
    vline (0,4) 11
    ]),
    ('B',Pictures [
    hline (0,4) 6,
    hline (0,10) 6,
    hline (0,14) 6,
    vline (6,5) 5,
    vline (6,11) 3,
    vline (0,4) 11
    ]),
    ('C',Pictures [
    hline (0,14) 7,
    hline (0,4) 7,
    vline (0,4) 11 
    ]),
    ('D',Pictures [
    hline (0,14) 6,
    hline (0,4) 6,
    vline (0,4) 11,
    vline (6,5) 9
    ]),
    ('E',Pictures [
    hline (0,14) 7,
    hline (0,10) 7,
    hline (0,4) 7,
    vline (0,4) 11
    ]),
    ('F',Pictures [
    hline (0,14) 7,
    hline (0,10) 7,
    vline (0,4) 11
    ]),
    ('G',Pictures [
    hline (0,4) 7,
    hline (4,10) 3,
    hline (0,14) 7,
    vline (6,4) 7,
    vline (0,4) 11
    ]),
    ('H',Pictures [
    hline (0,10) 7,
    vline (0,4) 11,
    vline (6,4) 11
    ]),
    ('I',Pictures [
    hline (2,14) 3,
    hline (2,4) 3,
    vline (3,4) 11
    ]),
    ('J',Pictures [
    hline (0,4) 7,
    vline (6,4) 11
    ]),
    ('K',Pictures [
    vline (0,4) 11,
    pix (1,9),
    pix (2,10),
    pix (2,8),
    pix (3,11),
    pix (3,7),
    pix (4,12),
    pix (4,6),
    pix (5,13),
    pix (5,5),
    pix (6,14),
    pix (6,4)
    ]),
    ('L',Pictures [
    vline (0,4) 11,
    hline (0,4) 7
    ]),
    ('M',Pictures [
    vline (0,4) 11,
    vline (6,4) 11,
    pix (1,13),
    pix (5,13),
    pix (2,12),
    pix (4,12),
    pix (3,11)
    ]),
    ('N',Pictures [
    vline (0,4) 11,
    vline (6,4) 11,
    pix (1,13),
    pix (2,12),
    pix (3,11),
    pix (4,10),
    pix (5,9)
    ]),
    ('O',Pictures [
    hline (0,14) 7,
    hline (0,4) 7,
    vline (0,4) 11,
    vline (6,4) 11 
    ]),
    ('P',Pictures [
    hline (0,14) 7,
    hline (0,10) 7,
    vline (0,4) 11,
    vline (6,10) 5
    ]),
    ('Q',Pictures [
    hline (0,14) 7,
    hline (0,4) 7,
    vline (0,4) 11,
    vline (6,3) 12 
    ]),
    ('R',Pictures [
    hline (0,14) 7,
    hline (0,10) 7,
    vline (0,4) 11,
    vline (6,10) 5,
    pix (1,9),
    pix (2,8),
    pix (3,7),
    pix (4,6),
    pix (5,5),
    pix (6,4)
    ]),
    ('S',Pictures [
    hline (0,14) 7,
    hline (0,10) 7,
    hline (0,4) 7,
    vline (6,4) 7,
    vline (0,10) 5,
    vline (0,4) 2,
    vline (6,13) 2
    ]),
    ('T',Pictures [
    hline (0,14) 7,
    vline (3,4) 11
    ]),
    ('U',Pictures [
    hline (0,4) 7,
    vline (0,4) 11,
    vline (6,4) 11
    ]),
    ('V',Pictures [
    vline (0,4) 11,
    vline (6,10) 5,
    pix (1,5),
    pix (2,6),
    pix (3,7),
    pix (4,8),
    pix (5,9)
    ]),
    ('W',Pictures [
    hline (0,4) 7,
    vline (0,4) 11,
    vline (3,4) 11,
    vline (6,4) 11
    ]),
    ('X',Pictures [
    vline (0,4) 4,
    vline (6,4) 4,
    pix (2,9),
    pix (4,9),
    pix (1,8),
    pix (5,8),
    pix (3,10),
    pix (2,11),
    pix (4,11),
    pix (1,12),
    pix (5,12),
    vline (0,13) 2,
    vline (6,13) 2
    ]),
    ('Y',Pictures [
    hline (0,10) 7,
    vline (0,10) 5,
    vline (6,10) 5,
    vline (3,4) 7
    ]),
    ('Z',Pictures [
    hline (0,4) 7,
    hline (0,14) 7,
    vline (0,5) 2,
    pix (1,7),
    pix (2,8),
    pix (3,9),
    pix (4,10),
    pix (5,11),
    vline (6,12) 2
    ]),
    ('a',Pictures [
    hline (0,10) 7,
    hline (0,7) 7,
    hline (0,4) 7,
    vline (6,4) 7,
    vline (0,4) 4
    ]),
    ('b',Pictures [
    hline (0,10) 7,
    hline (0,4) 7,
    vline (0,4) 11,
    vline (6,4) 7
    ]),
    ('c',Pictures [
    hline (0,10) 7,
    hline (0,4) 7,
    vline (0,4) 7
    ]),
    ('d',Pictures [
    hline (0,10) 7,
    hline (0,4) 7,
    vline (0,4) 7,
    vline (6,4) 11
    ]),
    ('e',Pictures [
    hline (0,10) 7,
    hline (0,7) 7,
    hline (0,4) 7,
    vline (6,7) 4,
    vline (0,4) 7
    ]),
    ('f',Pictures [
    hline (1,11) 3,
    hline (2,14) 5,
    vline (2,4) 11
    ]),
    ('g',Pictures [
    hline (0,10) 7,
    hline (0,4) 7,
    vline (0,4) 7,
    vline (6,0) 11,
    hline (0,0) 7
    ]),
    ('h',Pictures [
    hline (0,10) 7,
    vline (0,4) 11,
    vline (6,4) 7
    ]),
    ('i',Pictures [
    vline (3,4) 7,
    vline (3,12) 2
    ]),
    ('j',Pictures [
    hline (1,0) 3,
    vline (3,0) 11,
    vline (3,12) 2
    ]),
    ('k',Pictures [
    vline (0,4) 11,
    hline (0,7) 4,
    pix (4,6),
    pix (4,8),
    pix (5,5),
    pix (5,9),
    pix (6,4),
    pix (6,10)
    ]),
    ('l',Pictures [
    vline (2,4) 11,
    hline (2,4) 3
    ]),
    ('m',Pictures [
    hline (0,10) 7,
    vline (0,4) 7,
    vline (3,4) 7,
    vline (6,4) 7
    ]),
    ('n',Pictures [
    hline (0,10) 7,
    vline (0,4) 7,
    vline (6,4) 7
    ]),
    ('o',Pictures [
    hline (0,10) 7,
    hline (0,4) 7,
    vline (0,4) 7,
    vline (6,4) 7
    ]),
    ('p',Pictures [
    hline (0,10) 7,
    hline (0,4) 7,
    vline (0,0) 11,
    vline (6,4) 7
    ]),
    ('q',Pictures [
    hline (0,10) 7,
    hline (0,4) 7,
    vline (0,4) 7,
    vline (6,0) 11 
    ]),
    ('r',Pictures [
    hline (0,10) 7,
    vline (0,4) 7
    ]),
    ('s',Pictures [
    hline (0,10) 7,
    hline (0,7) 7,
    hline (0,4) 7,
    vline (6,4) 4,
    vline (0,7) 4
    ]),
    ('t',Pictures [
    hline (1,11) 5,
    hline (2,4) 5,
    vline (2,4) 11
    ]),
    ('u',Pictures [
    hline (0,4) 7,
    vline (0,4) 7,
    vline (6,4) 7
    ]),
    ('v',Pictures [
    hline (0,4) 4,
    vline (0,4) 7,
    vline (6,7) 4,
    pix (4,5),
    pix (5,6)
    ]),
    ('w',Pictures [
    hline (0,4) 7,
    vline (0,4) 7,
    vline (3,4) 7,
    vline (6,4) 7
    ]),
    ('x',Pictures [
    pix (0,4),
    pix (0,10),
    pix (1,5),
    pix (1,9),
    pix (2,6),
    pix (2,8),
    pix (3,7),
    pix (4,6),
    pix (4,8),
    pix (5,5),
    pix (5,9),
    pix (6,4),
    pix (6,10)
    ]),
    ('y',Pictures [
    hline (0,4) 7,
    vline (0,4) 7,
    vline (6,0) 11,
    hline (0,0) 7
    ]),
    ('z',Pictures [
    hline (0,4) 7,
    hline (0,10) 7,
    pix (1,5),
    pix (2,6),
    pix (3,7),
    pix (4,8),
    pix (5,9)
    ]),
-- digits
    ('0',Pictures [
    hline (0,14) 7,
    hline (0,4) 7,
    vline (0,4) 11,
    vline (6,4) 11,
    vline (3,7) 5
    ]),
    ('1',Pictures [
    vline (3,4) 11 
    ]),
    ('2',Pictures [
    hline (0,14) 7,
    hline (0,10) 7,
    hline (0,4) 7,
    vline (0,4) 7,
    vline (6,10) 5
    ]),
    ('3',Pictures [
    hline (0,14) 7,
    hline (0,10) 7,
    hline (0,4) 7,
    vline (6,4) 11 
    ]),
    ('4',Pictures [
    vline (0,10) 5,
    hline (0,10) 7,
    vline (6,4) 11
    ]),
    ('5',Pictures [
    hline (0,14) 7,
    hline (0,10) 7,
    hline (0,4) 7,
    vline (0,10) 5,
    vline (6,4) 7
    ]),
    ('6',Pictures [
    hline (0,14) 7,
    hline (0,10) 7,
    hline (0,4) 7,
    vline (0,4) 11,
    vline (6,4) 7
    ]),
    ('7',Pictures [
    hline (0,14) 7,
    vline (6,4) 11
    ]),
    ('8',Pictures [
    hline (0,14) 7,
    hline (0,10) 7,
    hline (0,4) 7,
    vline (0,4) 11,
    vline (6,4) 11
    ]),
    ('9',Pictures [
    hline (0,14) 7,
    hline (0,10) 7,
    hline (0,4) 7,
    vline (0,10) 5,
    vline (6,4) 11
    ]),
-- Punctuation and so on
    (' ', Blank),
    (',', vline (0,3) 3),
    ('.', Pictures[
        vline (0,4) 2,
        vline (1,4) 2
    ]),
    (':', Pictures[
    vline (1,5) 2,
    vline (1,9) 2
    ]),
    ('-', hline (0,7) 7),
    ('+', Pictures [
    hline (0,7) 7,
    vline (3,4) 7
    ]),
    ('<', Pictures[
    pix (0,7),
    hline (1,8) 2,
    hline (1,6) 2,
    hline (3,9) 2,
    hline (3,5) 2,
    hline (5,10) 2,
    hline (5,4) 2
    ]),
    ('>', Pictures[
    pix (6,7),
    hline (4,8) 2,
    hline (4,6) 2,
    hline (2,9) 2,
    hline (2,5) 2,
    hline (0,10) 2,
    hline (0,4) 2
    ]),
    ('!', Pictures [
    vline (3,7) 8,
    vline (3,4) 2
    ]),
    ('?', Pictures [
    hline (0,14) 7,
    hline (0,10) 7,
    hline (0,7) 7,
    vline (0,7) 3,
    vline (6,10) 4,
    vline (3,4) 2
    ]),
    ('|', Pictures [
    vline (3,0) 15
    ]),
    ('\"', Pictures [
        vline (2,11) 4,
        vline (4,11) 4
    ]),
    ('\'',vline (3,11) 4)
    ]

renderText :: String -> Picture
renderText = Pictures . zipWith (\o c -> Translate o 0 $ M.findWithDefault (rect (0,4) (7,7)) c chars) [0,8..]

renderWindow :: StateStack -> Window -> TAIO Picture
renderWindow ss win = do
    (cw,ch) <- uses (settings.dimensions) (bimap fromIntegral fromIntegral)
    (fw,fh) <- uses (settings.fontDimensions) (bimap fromIntegral fromIntegral)
    x <- fromIntegral <$> resolveLocation X (win^.left)
    y <- fromIntegral <$> resolveLocation Y (win^.top)
    x' <- fromIntegral <$> resolveLocation X (win^.right) 
    y' <- fromIntegral <$> resolveLocation Y (win^.bottom)
    let w = x'-x + 1
    let iW = round w - 2
    let h = y'-y + 1
    let iH = round h - 2
    let v = win^.view
    let hnd = fromIntegral $ win^.handle
    et <- use elapsedTime
    let cursorColor = if mod (floor (et*0.7)) 2 == 1 then green else black
    fs <- get
    return $ Translate (fw*(x-cw/2)) (fh*(-y+ch/2)) $ 
        Pictures [
            Color black $ rect (0,0) (w*fw,-h*fh),
            Translate 1 (-fh) $ Color green $ renderText ('+':replicate iW '-' ++"+"),
            Pictures $ map (\o -> Translate 1 (-o*fh) $ Color green $ renderText ('|':replicate iW ' ' ++"|")) [2..h-1],
            Translate 1 (-h*fh) $ Color green $ renderText ('+':replicate iW '-' ++"+"),
            Pictures (zipWith (\o -> Translate (fw+1) (-fh*o) . Color green . renderText) [2..] ((\ts -> drop (length ts - iH) ts) (v ss fs))),
            if hnd /= 0 then Blank else 
                let cy = fromIntegral $ length (v ss fs) in
                let cx = fromIntegral $ length (last (v ss fs)) in
                Translate ((cx+1)*fw+1) (-(cy+1)*fh) $ Color cursorColor $ rect (0,0) (fw-1,fh-1)
            ]