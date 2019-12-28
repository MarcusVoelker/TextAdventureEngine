module Frontend.Frontend where

import Frontend.Canvas
import Frontend.CanvasRenderer
import Frontend.Primitives
import Frontend.State
import Frontend.TAIO
import Frontend.Window

import Logic.Dialogue
import Logic.Driver
import Logic.GameState
import Logic.Response
import Logic.StateStack

import Engine

import Control.Exception
import Control.Lens hiding (view)
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Char
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import System.Exit
import System.IO (hFlush, stdout)

import Graphics.Gloss.Interface.IO.Game

openTopWindow :: ScreenLoc -> ScreenLoc -> ScreenLoc -> ScreenLoc -> View -> Int -> TAIO ()
openTopWindow x y w h v c = do
    hand <- (+1) . maximum . M.keys <$> use windows
    let win = Window hand x y w h v c
    windows %= M.insert hand win

closeContextWindows :: Int -> TAIO ()
closeContextWindows c = windows %= M.filter (\w -> w^.context /= c)

executeResponse :: StateStack -> Response -> TAIO StateStack
executeResponse ss (TextResponse s) = do
    textHistory %= (++lines s)
    return ss
executeResponse ss (InitiateDialogueResponse d) = do
    openTopWindow 
        (Absolute 2)
        (Absolute 2) 
        (Absolute (-4)) 
        (Absolute (-8))
        (\ss _ -> [head(ss^.stack)^.dialogue.response])
        (contextCount ss + 1)
    return $ openContext (DialogueState d) ss
executeResponse ss LeaveContextResponse = do
    closeContextWindows $ contextCount ss
    return $ closeContext ss
executeResponse ss QuitResponse = lift exitSuccess

executeResponses :: Responding StateStack -> TAIO StateStack
executeResponses (Responding responses ss) = foldM executeResponse ss responses

initialInputState :: InputState 
initialInputState = InputState [] [] 0 Nothing

initialFrontendState :: (Int,Int) -> (Int,Int) -> FrontendState
initialFrontendState (w,h) (fw,fh) = FrontendState 
    [] 
    initialInputState
    initialCanvasState
    (M.fromList [
        (0,Window 0 (Absolute 0) (Absolute (-3)) (Absolute (-1)) (Absolute (-1)) (\_ fs -> ["> " ++ (fs^.input.buffer)]) 0),
        (1,Window 1 (Absolute 0) (Absolute 0) (Absolute (-1)) (Absolute (-3)) (\_ fs -> fs^.textHistory) 0)
        ]) 
    (FrontendSettings (w,h) (fw,fh))
    0

runFrontend :: (Int,Int) -> (Int,Int) -> TAIO () -> IO ()
runFrontend dims fdims action = do 
    let irs = initialFrontendState dims fdims
    void $ execStateT action irs

eventHandler :: Event -> (StateStack,FrontendState) -> IO (StateStack,FrontendState)
eventHandler e (ss,fs) = runStateT (handleEvent e ss) fs

handleEvent :: Event -> StateStack -> TAIO StateStack
handleEvent (EventKey (Char c) Down m _) ss 
    | ord c == 8 = do
        nn <- uses (input.buffer) (not.null)
        when nn $
            (input.buffer) %= init
        return ss
    | otherwise = do
        (input.buffer) %= (++[if shift m == Down then toUpper c else c])
        return ss 
handleEvent (EventKey (SpecialKey KeySpace) Down _ _) ss = do
    (input.buffer) %= (++[' '])
    return ss
handleEvent (EventKey (SpecialKey KeyShiftR) Down _ _) ss = return ss
handleEvent (EventKey (SpecialKey KeyUp) Down _ _) ss = do
    st <- use (input.search)
    when (isNothing st) $ do
        curCode <- use (input.buffer)
        (input.search) .= Just curCode
    searchTerm <- use (input.search)
    ip <- use (input.historyPointer)
    prefix <- uses (input.history) (reverse . take ip)
    case find (fromJust searchTerm `isPrefixOf`) prefix of
        Nothing -> return ss
        Just pCode -> do
            (input.historyPointer) .= fromJust (elemIndex pCode (reverse prefix))
            (input.buffer) .= pCode
            return ss
handleEvent (EventKey (SpecialKey KeyEnter) Down _ _) ss = do
    code <- use (input.buffer)
    (input.history) %= (++[code])
    ihLen <- uses (input.history) length
    (input.historyPointer) .= ihLen
    (input.buffer) .= ""
    (input.search) .= Nothing
    executeResponses $ executeCommand code ss
handleEvent (EventResize (x,y)) ss = do
    (fx,fy) <- use $ settings.fontDimensions
    let nx = div x fx
    let ny = div y fy
    (settings.dimensions) .= (nx,ny)
    return ss
handleEvent (EventKey _ Up _ _) ss = return ss
handleEvent (EventKey (MouseButton _) _ _ _) ss = return ss
handleEvent (EventMotion _) ss = return ss
handleEvent e ss = do
    lift $ putStrLn $ "Unhandled Event " ++ show e
    return ss

updateHandler :: Float -> (StateStack,FrontendState) -> IO (StateStack,FrontendState)
updateHandler t (ss,fs) = runStateT (stepFrontend t ss) fs

stepFrontend :: Float -> StateStack -> TAIO StateStack
stepFrontend t ss = do
    elapsedTime %= (+t)
    return ss

renderHandler :: (StateStack,FrontendState) -> IO Picture
renderHandler (ss,fs) = evalStateT (renderFrontend ss) fs

screenEffect :: TAIO Picture
screenEffect = do
    (cw,ch) <- uses (settings.dimensions) (bimap fromIntegral fromIntegral)
    (fw,fh) <- uses (settings.fontDimensions) (bimap fromIntegral fromIntegral)
    let w = fw*cw
    let h = fh*ch
    return $ Translate (fw*(-cw/2)) (fh*(ch/2)) $ 
        Pictures $ map (\y -> Color (makeColor 0 (fromIntegral (mod y 2)) 0 0.1) $ rect (0,-2*fromIntegral y) (w,2)) [0..div (round h-1) 2]

renderFrontend :: StateStack -> TAIO Picture
renderFrontend ss = do
    ws <- uses windows M.elems
    se <- screenEffect
    mapM_ (renderWindow ss) ws
    cv <- renderCanvas
    return $ Pictures [se,cv]