module Frontend.Text where

import Frontend.Primitives

import qualified Data.Map.Strict as M

import Graphics.UI.GLUT hiding (rect)

bSHLine :: Char
bSHLine = '─'

bSVLine :: Char
bSVLine = '│'

bSTRCorner :: Char
bSTRCorner = '┐'

bSTLCorner :: Char
bSTLCorner = '┌'

bSBRCorner :: Char
bSBRCorner = '┘'

bSBLCorner :: Char
bSBLCorner = '└'

bDHLine :: Char
bDHLine = '═'

bDVLine :: Char
bDVLine = '║'

bDTRCorner :: Char
bDTRCorner = '╗'

bDTLCorner :: Char
bDTLCorner = '╔'

bDBRCorner :: Char
bDBRCorner = '╝'

bDBLCorner :: Char
bDBLCorner = '╚'

-- ┤	╡	╢	╖	╕	╣				╜	╛	
-- 	┴	┬	├		┼	╞	╟			╩	╦	╠		╬	╧
-- ╨	╤	╥	╙	╘	╒	╓	╫	╪		


chars :: M.Map Char (IO ())
chars = M.fromList [
    ('A',sequence_ [
    hline (0,10) 7,
    hline (0,14) 7,
    vline (6,4) 11,
    vline (0,4) 11
    ]),
    ('B',sequence_ [
    hline (0,4) 6,
    hline (0,10) 6,
    hline (0,14) 6,
    vline (6,5) 5,
    vline (6,11) 3,
    vline (0,4) 11
    ]),
    ('C',sequence_ [
    hline (0,14) 7,
    hline (0,4) 7,
    vline (0,4) 11 
    ]),
    ('D',sequence_ [
    hline (0,14) 6,
    hline (0,4) 6,
    vline (0,4) 11,
    vline (6,5) 9
    ]),
    ('E',sequence_ [
    hline (0,14) 7,
    hline (0,10) 7,
    hline (0,4) 7,
    vline (0,4) 11
    ]),
    ('F',sequence_ [
    hline (0,14) 7,
    hline (0,10) 7,
    vline (0,4) 11
    ]),
    ('G',sequence_ [
    hline (0,4) 7,
    hline (4,10) 3,
    hline (0,14) 7,
    vline (6,4) 7,
    vline (0,4) 11
    ]),
    ('H',sequence_ [
    hline (0,10) 7,
    vline (0,4) 11,
    vline (6,4) 11
    ]),
    ('I',sequence_ [
    hline (2,14) 3,
    hline (2,4) 3,
    vline (3,4) 11
    ]),
    ('J',sequence_ [
    hline (0,4) 7,
    vline (6,4) 11
    ]),
    ('K',sequence_ [
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
    ('L',sequence_ [
    vline (0,4) 11,
    hline (0,4) 7
    ]),
    ('M',sequence_ [
    vline (0,4) 11,
    vline (6,4) 11,
    pix (1,13),
    pix (5,13),
    pix (2,12),
    pix (4,12),
    pix (3,11)
    ]),
    ('N',sequence_ [
    vline (0,4) 11,
    vline (6,4) 11,
    pix (1,13),
    pix (2,12),
    pix (3,11),
    pix (4,10),
    pix (5,9)
    ]),
    ('O',sequence_ [
    hline (0,14) 7,
    hline (0,4) 7,
    vline (0,4) 11,
    vline (6,4) 11 
    ]),
    ('P',sequence_ [
    hline (0,14) 7,
    hline (0,10) 7,
    vline (0,4) 11,
    vline (6,10) 5
    ]),
    ('Q',sequence_ [
    hline (0,14) 7,
    hline (0,4) 7,
    vline (0,4) 11,
    vline (6,3) 12 
    ]),
    ('R',sequence_ [
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
    ('S',sequence_ [
    hline (0,14) 7,
    hline (0,10) 7,
    hline (0,4) 7,
    vline (6,4) 7,
    vline (0,10) 5,
    vline (0,4) 2,
    vline (6,13) 2
    ]),
    ('T',sequence_ [
    hline (0,14) 7,
    vline (3,4) 11
    ]),
    ('U',sequence_ [
    hline (0,4) 7,
    vline (0,4) 11,
    vline (6,4) 11
    ]),
    ('V',sequence_ [
    vline (0,4) 11,
    vline (6,10) 5,
    pix (1,5),
    pix (2,6),
    pix (3,7),
    pix (4,8),
    pix (5,9)
    ]),
    ('W',sequence_ [
    hline (0,4) 7,
    vline (0,4) 11,
    vline (3,4) 11,
    vline (6,4) 11
    ]),
    ('X',sequence_ [
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
    ('Y',sequence_ [
    hline (0,10) 7,
    vline (0,10) 5,
    vline (6,10) 5,
    vline (3,4) 7
    ]),
    ('Z',sequence_ [
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
    ('a',sequence_ [
    hline (0,10) 7,
    hline (0,7) 7,
    hline (0,4) 7,
    vline (6,4) 7,
    vline (0,4) 4
    ]),
    ('b',sequence_ [
    hline (0,10) 7,
    hline (0,4) 7,
    vline (0,4) 11,
    vline (6,4) 7
    ]),
    ('c',sequence_ [
    hline (0,10) 7,
    hline (0,4) 7,
    vline (0,4) 7
    ]),
    ('d',sequence_ [
    hline (0,10) 7,
    hline (0,4) 7,
    vline (0,4) 7,
    vline (6,4) 11
    ]),
    ('e',sequence_ [
    hline (0,10) 7,
    hline (0,7) 7,
    hline (0,4) 7,
    vline (6,7) 4,
    vline (0,4) 7
    ]),
    ('f',sequence_ [
    hline (1,11) 3,
    hline (2,14) 5,
    vline (2,4) 11
    ]),
    ('g',sequence_ [
    hline (0,10) 7,
    hline (0,4) 7,
    vline (0,4) 7,
    vline (6,0) 11,
    hline (0,0) 7
    ]),
    ('h',sequence_ [
    hline (0,10) 7,
    vline (0,4) 11,
    vline (6,4) 7
    ]),
    ('i',sequence_ [
    vline (3,4) 7,
    vline (3,12) 2
    ]),
    ('j',sequence_ [
    hline (1,0) 3,
    vline (3,0) 11,
    vline (3,12) 2
    ]),
    ('k',sequence_ [
    vline (0,4) 11,
    hline (0,7) 4,
    pix (4,6),
    pix (4,8),
    pix (5,5),
    pix (5,9),
    pix (6,4),
    pix (6,10)
    ]),
    ('l',sequence_ [
    vline (2,4) 11,
    hline (2,4) 3
    ]),
    ('m',sequence_ [
    hline (0,10) 7,
    vline (0,4) 7,
    vline (3,4) 7,
    vline (6,4) 7
    ]),
    ('n',sequence_ [
    hline (0,10) 7,
    vline (0,4) 7,
    vline (6,4) 7
    ]),
    ('o',sequence_ [
    hline (0,10) 7,
    hline (0,4) 7,
    vline (0,4) 7,
    vline (6,4) 7
    ]),
    ('p',sequence_ [
    hline (0,10) 7,
    hline (0,4) 7,
    vline (0,0) 11,
    vline (6,4) 7
    ]),
    ('q',sequence_ [
    hline (0,10) 7,
    hline (0,4) 7,
    vline (0,4) 7,
    vline (6,0) 11 
    ]),
    ('r',sequence_ [
    hline (0,10) 7,
    vline (0,4) 7
    ]),
    ('s',sequence_ [
    hline (0,10) 7,
    hline (0,7) 7,
    hline (0,4) 7,
    vline (6,4) 4,
    vline (0,7) 4
    ]),
    ('t',sequence_ [
    hline (1,11) 5,
    hline (2,4) 5,
    vline (2,4) 11
    ]),
    ('u',sequence_ [
    hline (0,4) 7,
    vline (0,4) 7,
    vline (6,4) 7
    ]),
    ('v',sequence_ [
    hline (0,4) 4,
    vline (0,4) 7,
    vline (6,7) 4,
    pix (4,5),
    pix (5,6)
    ]),
    ('w',sequence_ [
    hline (0,4) 7,
    vline (0,4) 7,
    vline (3,4) 7,
    vline (6,4) 7
    ]),
    ('x',sequence_ [
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
    ('y',sequence_ [
    hline (0,4) 7,
    vline (0,4) 7,
    vline (6,0) 11,
    hline (0,0) 7
    ]),
    ('z',sequence_ [
    hline (0,4) 7,
    hline (0,10) 7,
    pix (1,5),
    pix (2,6),
    pix (3,7),
    pix (4,8),
    pix (5,9)
    ]),
-- digits
    ('0',sequence_ [
    hline (0,14) 7,
    hline (0,4) 7,
    vline (0,4) 11,
    vline (6,4) 11,
    vline (3,7) 5
    ]),
    ('1',sequence_ [
    vline (3,4) 11 
    ]),
    ('2',sequence_ [
    hline (0,14) 7,
    hline (0,10) 7,
    hline (0,4) 7,
    vline (0,4) 7,
    vline (6,10) 5
    ]),
    ('3',sequence_ [
    hline (0,14) 7,
    hline (0,10) 7,
    hline (0,4) 7,
    vline (6,4) 11 
    ]),
    ('4',sequence_ [
    vline (0,10) 5,
    hline (0,10) 7,
    vline (6,4) 11
    ]),
    ('5',sequence_ [
    hline (0,14) 7,
    hline (0,10) 7,
    hline (0,4) 7,
    vline (0,10) 5,
    vline (6,4) 7
    ]),
    ('6',sequence_ [
    hline (0,14) 7,
    hline (0,10) 7,
    hline (0,4) 7,
    vline (0,4) 11,
    vline (6,4) 7
    ]),
    ('7',sequence_ [
    hline (0,14) 7,
    vline (6,4) 11
    ]),
    ('8',sequence_ [
    hline (0,14) 7,
    hline (0,10) 7,
    hline (0,4) 7,
    vline (0,4) 11,
    vline (6,4) 11
    ]),
    ('9',sequence_ [
    hline (0,14) 7,
    hline (0,10) 7,
    hline (0,4) 7,
    vline (0,10) 5,
    vline (6,4) 11
    ]),
-- Punctuation and so on
    (' ', return ()),
    (',', vline (0,3) 3),
    ('.', sequence_[
        vline (0,4) 2,
        vline (1,4) 2
    ]),
    (':', sequence_[
    vline (1,5) 2,
    vline (1,9) 2
    ]),
    ('-', hline (0,7) 7),
    ('+', sequence_ [
    hline (0,7) 7,
    vline (3,4) 7
    ]),
    ('<', sequence_[
    pix (0,7),
    hline (1,8) 2,
    hline (1,6) 2,
    hline (3,9) 2,
    hline (3,5) 2,
    hline (5,10) 2,
    hline (5,4) 2
    ]),
    ('>', sequence_[
    pix (6,7),
    hline (4,8) 2,
    hline (4,6) 2,
    hline (2,9) 2,
    hline (2,5) 2,
    hline (0,10) 2,
    hline (0,4) 2
    ]),
    ('!', sequence_ [
    vline (3,7) 8,
    vline (3,4) 2
    ]),
    ('?', sequence_ [
    hline (0,14) 7,
    hline (0,10) 7,
    hline (0,7) 7,
    vline (0,7) 3,
    vline (6,10) 4,
    vline (3,4) 2
    ]),
    ('|', sequence_ [
    vline (3,0) 15
    ]),
    ('\"', sequence_ [
        vline (2,11) 4,
        vline (4,11) 4
    ]),
    ('\'',vline (3,11) 4),
-- Box drawing
    (bSVLine, vline (3,0) 16),
    (bSHLine, hline (0,7) 8),
    (bSTLCorner, sequence_ [
        vline (3,0) 8,
        hline (4,7) 4
    ]),
    (bSTRCorner, sequence_ [
        vline (3,0) 8,
        hline (0,7) 4
    ]),
    (bSBLCorner, sequence_ [
        vline (3,7) 9,
        hline (3,7) 5
    ]),
    (bSBRCorner, sequence_ [
        vline (3,8) 8,
        hline (0,7) 4
    ]),
    (bDVLine, sequence_ [
        vline (2,0) 16,
        vline (5,0) 16
    ]),
    (bDHLine, sequence_[
        hline (0,6) 8,
        hline (0,9) 8
    ]),
    (bDTLCorner, sequence_ [
        vline (2,0) 9,
        vline (5,0) 6,
        hline (2,9) 6,
        hline (5,6) 3
    ]),
    (bDTRCorner, sequence_ [
        vline (2,0) 6,
        vline (5,0) 9,
        hline (0,9) 6,
        hline (0,6) 3
    ]),
    (bDBLCorner, sequence_ [
        vline (2,6) 10,
        vline (5,9) 7,
        hline (2,6) 6,
        hline (5,9) 3
    ]),
    (bDBRCorner, sequence_ [
        vline (2,9) 7,
        vline (5,6) 10,
        hline (0,9) 3,
        hline (0,6) 6
    ])
    ]

renderChar :: Char -> IO ()
renderChar c = do
    M.findWithDefault (rect (0,4) (7,7)) c chars

renderText :: String -> IO ()
renderText = sequence_ . zipWith (\o c -> translate (Vector3 (fromIntegral o :: GLfloat) 0 0) >> renderChar c) [0,8..]