module MainLoop where

import Parser.ConfigParser
import Parser.EntityParser
import Parser.ItemParser
import Parser.RoomParser
import Parser.VariableParser
import Parser.Tokenizer
import Logic.Deserialiser (DeserialisationContext,DeserialisationContext(DeserialisationContext))
import Logic.EntityKind
import Logic.GameState
import Logic.Interaction
import Logic.Response
import Logic.StateStack
import Sound.Engine
import Engine
import Thing
import Frontend.Frontend

import Text.LParse.Parser

import qualified Graphics.UI.GLFW as GLFW
import Graphics.GL
import Graphics.GLU ( gluPerspective )

import Control.Arrow hiding (left)
import Control.DoubleContinuations
import Control.Lens hiding (view)
import Control.Monad.Trans.State
import Data.Maybe
import qualified Data.Map.Strict as M
import Data.Bits ( (.|.) )
import System.Exit (exitSuccess)
import Control.Monad ( forever )

entityMap :: [EntityKind] -> M.Map String EntityKind
entityMap = M.fromList . map (\ek -> (ek^.idt,ek))

fullParser :: String -> String -> String -> String -> String -> DCont r String (Maybe (Int,Int),String,StateStack,DeserialisationContext)
fullParser r e i c v = do
    rMap <- fst <$> pFunc (tokenizer >>> blocker >>> rooms) r
    iMap <- fst <$> pFunc (tokenizer >>> blocker >>> items) i
    es <- fst <$> pFunc (tokenizer >>> blocker >>> Parser.EntityParser.entities rMap iMap) e
    (d,t,iR) <- fst <$> pFunc (tokenizer >>> blocker >>> config) c
    vs <- fst <$> pFunc (tokenizer >>> blocker >>> Parser.VariableParser.variables) v
    let initial = initialState (fromJust $ M.lookup iR rMap) vs
    return $ (d,t,,DeserialisationContext rMap (entityMap $ map snd es) iMap) $ (^.result) $ buildStateStack <$> execStateT (mapM_ (uncurry instantiateEntity) (mapMaybe (\(r,e) -> (,e) <$> r) es)) initial

runGame :: IO ()
runGame = withEngine soundEngine $ do
    roomCode <- readFile "app/rooms.dat"
    entityCode <- readFile "app/entities.dat"
    itemCode <- readFile "app/items.dat"
    configCode <- readFile "app/config.dat"
    variableCode <- readFile "app/variables.dat"
    run (fullParser roomCode entityCode itemCode configCode variableCode) (\(d,t,ss,dc) -> mainOpenGL d t ss dc) putStrLn 
   
initGL :: GLFW.Window -> IO ()
initGL win = do
  glShadeModel GL_SMOOTH -- enables smooth color shading
  glClearColor 0 0 0 0 -- Clear the background color to black
  glClearDepth 1 -- enables clearing of the depth buffer
  glEnable GL_DEPTH_TEST
  glDepthFunc GL_LEQUAL -- type of depth test
  glHint GL_PERSPECTIVE_CORRECTION_HINT GL_NICEST
  (w,h) <- GLFW.getFramebufferSize win
  resizeScene win w h

resizeScene :: GLFW.WindowSizeCallback
resizeScene win w     0      = resizeScene win w 1 -- prevent divide by zero
resizeScene _   width height = do
  glViewport 0 0 (fromIntegral width) (fromIntegral height)
  glMatrixMode GL_PROJECTION
  glLoadIdentity
  gluPerspective 45 (fromIntegral width/fromIntegral height) 0.1 100 
  glMatrixMode GL_MODELVIEW
  glLoadIdentity
  glFlush

drawScene :: GLFW.Window -> IO ()
drawScene _ = do
  -- clear the screen and the depth buffer
  glClear $ fromIntegral  $  GL_COLOR_BUFFER_BIT
                         .|. GL_DEPTH_BUFFER_BIT
  glLoadIdentity  -- reset view
  glFlush

shutdown :: GLFW.WindowCloseCallback
shutdown win = do
  GLFW.destroyWindow win
  GLFW.terminate
  _ <- exitSuccess
  return ()

keyPressed :: GLFW.KeyCallback 
keyPressed win GLFW.Key'Escape _ GLFW.KeyState'Pressed _ = shutdown win
keyPressed _   _               _ _                     _ = return ()

mainOpenGL :: Maybe (Int,Int) -> String -> StateStack -> DeserialisationContext -> IO ()
mainOpenGL dims title ss dc = do
    let (iow,ifs) = case dims of
            Just (w,h) -> (GLFW.createWindow w h title Nothing Nothing,initialFrontendState (div w 8,div h 16) (8,16) dc)
            Nothing -> (GLFW.createWindow 960 640 title Nothing Nothing,initialFrontendState (div 960 8,div 640 16) (8,16) dc)
    True <- GLFW.init
    GLFW.defaultWindowHints
    Just win <- iow
    GLFW.makeContextCurrent (Just win)
    GLFW.setWindowRefreshCallback win (Just drawScene)
    GLFW.setFramebufferSizeCallback win (Just resizeScene)
    GLFW.setKeyCallback win (Just keyPressed)
    GLFW.setWindowCloseCallback win (Just shutdown)
    initGL win
    forever $ do
        GLFW.pollEvents
        drawScene win
        GLFW.swapBuffers win
    {-(sw,sh) <- getScreenSize
    let (disp,ifs) = case dims of
            Just (w,h) -> (InWindow title (w+1,h+1) (div(sw-w)2,div(sh-h)2),initialFrontendState (div w 8,div h 16) (8,16) dc)
            Nothing -> (FullScreen,initialFrontendState (div sw 8,div sh 16) (8,16) dc)
    playIO
        disp
        black
        60
        (ss,ifs)
        renderHandler
        eventHandler
        updateHandler-}