module Frontend.Primitives where

import Graphics.Gloss

rect :: (Float,Float) -> (Float,Float) -> Picture
rect (x,y) (w,h) = Polygon [(x,y),(x+w,y),(x+w,y+h),(x,y+h)]

hline :: (Float,Float) -> Float -> Picture
hline (x,y) l = rect (x,y) (l,1)

vline :: (Float,Float) -> Float -> Picture
vline (x,y) l = rect (x,y) (1,l)

pix :: (Float,Float) -> Picture
pix (x,y) = rect (x,y) (1,1)
