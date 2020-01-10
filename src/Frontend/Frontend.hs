module Frontend.Frontend where

import Frontend.Canvas
import Frontend.CanvasRenderer
import Frontend.Primitives
import Frontend.State
import Frontend.FrontState
import Frontend.Window

import Logic.Dialogue
import Logic.Driver
import Logic.Response
import Logic.StateStack

import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Char
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import System.Exit

import Graphics.Gloss.Interface.IO.Game

openTopWindow :: ScreenLoc -> ScreenLoc -> ScreenLoc -> ScreenLoc -> ContentView -> Int -> FrontMod ()
openTopWindow x y w h v c = do
    hand <- (+1) . maximum . M.keys <$> use windows
    let win = Window hand x y w h v c singleStyle
    windows %= M.insert hand win

closeContextWindows :: Int -> FrontMod ()
closeContextWindows c = windows %= M.filter (\w -> w^.context /= c)

executeResponse :: StateStack -> Response -> FrontMod StateStack
executeResponse ss (TextResponse s) = do
    textHistory %= (++lines s)
    return ss
executeResponse ss (InitiateDialogueResponse d) = do
    let ss' = openContext (DialogueState d) ss
    openTopWindow 
        (Absolute 2)
        (Absolute 2) 
        (Absolute (-4)) 
        (Absolute (-8))
        (\ss _ -> case ss^.stack of 
            [] -> ["Nobody here but us chickens!"]
            (x:_) -> [x^.dialogue.response]
            )
        (contextCount ss + 1)
    return ss'
executeResponse ss LeaveContextResponse = do
    closeContextWindows $ contextCount ss
    return $ closeContext ss
executeResponse _ QuitResponse = lift exitSuccess
executeResponse _ _ = lift $ throwIO $ PatternMatchFail "Unhandled Response!"

executeResponses :: Responding StateStack -> FrontMod StateStack
executeResponses (Responding responses ss) = foldM executeResponse ss responses

initialInputState :: InputState 
initialInputState = InputState [] [] 0 Nothing

initialFrontendState :: (Int,Int) -> (Int,Int) -> FrontendState
initialFrontendState (w,h) (fw,fh) = FrontendState 
    [] 
    initialInputState
    initialCanvasState
    (M.fromList [
        (0,Window 0 (Absolute 0) (Absolute (-3)) (Absolute (-1)) (Absolute (-1)) (\_ fs -> ["> " ++ (fs^.input.buffer)]) 0 doubleStyle),
        (1,Window 1 (Absolute 0) (Absolute 0) (Absolute (-1)) (Absolute (-4)) (\_ fs -> 
            let (_,ch) = fs^.settings.dimensions
                th     = fs^.textHistory 
                in
            drop (max 0 (length th-ch+5)) th) 0 doubleStyle)
        ]) 
    (FrontendSettings (w,h) (fw,fh))
    0

eventHandler :: Event -> (StateStack,FrontendState) -> IO (StateStack,FrontendState)
eventHandler e (ss,fs) = runStateT (do
    r <- handleEvent e ss
    ws <- uses windows M.elems
    mapM_ (renderWindow ss) ws
    return r
    ) fs

handleEvent :: Event -> StateStack -> FrontMod StateStack
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
handleEvent (EventKey (SpecialKey KeyShiftL) Down _ _) ss = return ss
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

stepFrontend :: Float -> StateStack -> FrontMod StateStack
stepFrontend t ss = do
    elapsedTime %= (+t)
    return ss

renderHandler :: (StateStack,FrontendState) -> IO Picture
renderHandler (_,fs) = runReaderT renderFrontend fs

screenEffect :: FrontRead Picture
screenEffect = do
    (cw,ch) <- views (settings.dimensions) (bimap fromIntegral fromIntegral)
    (fw,fh) <- views (settings.fontDimensions) (bimap fromIntegral fromIntegral)
    let w = fw*cw
    let h = fh*ch
    return $ Translate (fw*(-cw/2)) (fh*(ch/2)) $ 
        Pictures $ map (\y -> Color (makeColor 0 (fromIntegral (mod y 2)) 0 0.1) $ rect (0,-2*fromIntegral y) (w,2)) [0..div (round h-1) 2]

renderFrontend :: FrontRead Picture
renderFrontend = do
    se <- screenEffect
    cv <- renderCanvas
    return $ Pictures [se,cv]