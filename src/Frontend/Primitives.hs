module Frontend.Primitives where

type Color = (Double,Double,Double,Double)
data Picture = Blank | Pictures [Picture] | Translate Float Float Picture | Color Color Picture

makeColor = (,,,)

green :: Color
green = undefined

rect :: (Float,Float) -> (Float,Float) -> Picture
rect = undefined

hline :: (Float,Float) -> Float -> Picture
hline = undefined

vline :: (Float,Float) -> Float -> Picture
vline = undefined

pix :: (Float,Float) -> Picture
pix = undefined
