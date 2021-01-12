module Frontend.Primitives where

import Graphics.UI.GLUT hiding (rect)

vertex3f :: (Integral a) => (a, a, a) -> IO ()
vertex3f (x, y, z) = vertex $ Vertex3 ((fromIntegral x) :: GLfloat) (fromIntegral y) (fromIntegral z)

rect :: (Int,Int) -> (Int,Int) -> IO ()
rect (xa,ya) (xb,yb) = renderPrimitive Quads $ do
    color $ Color3 (0 :: GLfloat) 1 0
    mapM_ vertex3f [(xa,ya,0),(xa,yb,0),(xb,yb,0),(xb,ya,0)]

hline :: (Int,Int) -> Int -> IO ()
hline (x,y) l = rect (x,y) (x+l,y+1)

vline :: (Int,Int) -> Int -> IO ()
vline (x,y) l = rect (x,y) (x+1,y+l)

pix :: (Int,Int) -> IO ()
pix (x,y) = rect (x,y) (x+1,y+1)