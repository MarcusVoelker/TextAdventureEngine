module Frontend.Frontend where

import Frontend.Canvas
import Frontend.CanvasRenderer
import Frontend.Primitives
import Frontend.State
import Frontend.FrontState
import Frontend.Window

import GameData.Text hiding (Color)

import Logic.Deserialiser
import Logic.Dialogue
import Logic.Driver
import Logic.GameState
import Logic.Menu
import Logic.Response
import Logic.StateStack

import Serialiser
import Thing

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

import Graphics.Gloss.Interface.IO.Game hiding (Text)

openTopWindow :: ScreenLoc -> ScreenLoc -> ScreenLoc -> ScreenLoc -> ContentView -> Int -> FrontMod ()
openTopWindow x y w h v c = do
    hand <- (+1) . maximum . M.keys <$> use windows
    let win = Window hand x y w h v c singleStyle
    windows %= M.insert hand win

closeContextWindows :: Int -> FrontMod ()
closeContextWindows c = windows %= M.filter (\w -> w^.context /= c)

renderMenu :: StackedState -> [ResolvedText]
renderMenu (MenuState menu idx) = 
    zip [a == idx|a<-[0..]] (menu^.content) 
    >>= (\case 
        (False,c) -> [liftString "",Text " " : resolveText M.empty (c^.name),liftString ""]
        (True,c) -> let rt = resolveText M.empty (c^.name) in
            [liftString (bSTLCorner : (bSHLine <$ [1..textLength rt]) ++ [bSTRCorner]),
            Text [bSVLine] : rt ++ [Text [bSVLine]],
            liftString (bSBLCorner : (bSHLine <$ [1..textLength rt]) ++ [bSBRCorner])
            ]
    )

bSHLine = '─'
bSVLine = '│'
bSTRCorner = '┐'
bSTLCorner = '┌'
bSBRCorner = '┘'
bSBLCorner = '└'

openMainMenu :: StateStack -> FrontMod StateStack
openMainMenu ss = do
    let ss' = openContext (MenuState mainMenu 0) ss
    openTopWindow 
        (Absolute 0)
        (Absolute 0) 
        (Absolute (-1)) 
        (Absolute (-1))
        (\ss _ -> case ss^.stack of 
            [] -> [liftString "Nobody here but us chickens!"]
            (x:_) -> renderMenu x 
            )
        (contextCount ss + 1)
    return ss'

executeResponse :: StateStack -> Response -> FrontMod StateStack
executeResponse ss (TextResponse s) = do
    textHistory %= (++[s])
    return ss
executeResponse ss (InitiateDialogueResponse d) = do
    let ss' = openContext (DialogueState d) ss
    openTopWindow 
        (Absolute 2)
        (Absolute 2) 
        (Absolute (-4)) 
        (Absolute (-8))
        (\ss _ -> case ss^.stack of 
            [] -> [liftString "Nobody here but us chickens!"]
            (x:_) -> [liftString $ x^.dialogue.response]
            )
        (contextCount ss + 1)
    return ss'
executeResponse ss (OpenMenuResponse "main") = openMainMenu ss
executeResponse ss LeaveContextResponse = do
    closeContextWindows $ contextCount ss
    return $ closeContext ss
executeResponse ss SaveResponse = lift (saveObject "save.dat" (ss^.bottom)) >> return ss
executeResponse _ LoadResponse = do
    dc <- use deserialisationContext
    (\gs -> StateStack gs []) <$> lift (loadObject "save.dat" dc)
executeResponse _ QuitResponse = lift exitSuccess
executeResponse _ _ = lift $ throwIO $ PatternMatchFail "Unhandled Response!"

executeResponses :: Responding StateStack -> FrontMod StateStack
executeResponses (Responding responses ss) = foldM executeResponse ss responses

initialInputState :: InputState 
initialInputState = InputState [] [] 0 Nothing

initialFrontendState :: (Int,Int) -> (Int,Int) -> DeserialisationContext -> FrontendState
initialFrontendState (w,h) (fw,fh) = FrontendState 
    [] 
    initialInputState
    initialCanvasState
    (M.fromList [
        (0,Window 0 (Absolute 0) (Absolute (-3)) (Absolute (-1)) (Absolute (-1)) (\_ fs -> [liftString $ "> " ++ (fs^.input.buffer)]) 0 doubleStyle),
        (1,Window 1 (Absolute 0) (Absolute 0) (Absolute (-1)) (Absolute (-4)) (\_ fs -> 
            let (_,ch) = fs^.settings.dimensions
                th     = fs^.textHistory 
                in
            drop (max 0 (length th-ch+5)) th) 0 singleStyle)
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
handleEvent e ss | noContext ss = handleMainEvent e ss
                 | otherwise    = handleMenuEvent e ss

handleMenuEvent :: Event -> StateStack -> FrontMod StateStack
handleMenuEvent (EventKey (SpecialKey KeyUp) Down _ _) ss@(StateStack _ (MenuState menu idx:_)) = do
    let menuSize = length (menu^.content)
    return $ ss&stackTop %~ (\(MenuState menu idx) -> MenuState menu (mod (idx + menuSize - 1) menuSize))
handleMenuEvent (EventKey (SpecialKey KeyDown) Down _ _) ss@(StateStack _ (MenuState menu idx:_)) = do
    let menuSize = length (menu^.content)
    return $ ss&stackTop %~ (\(MenuState menu idx) -> MenuState menu (mod (idx + 1) menuSize))
handleMenuEvent _ ss@(StateStack _ (MenuState _ _:_)) = do
    lift $ putStrLn "Unhandled Menu State Event"
    return ss
handleMenuEvent _ ss = do
    lift $ putStrLn "Unhandled Menu State"
    return ss

handleMainEvent :: Event -> StateStack -> FrontMod StateStack
handleMainEvent (EventKey (Char c) Down m _) ss 
    | ord c == 8 = do
        nn <- uses (input.buffer) (not.null)
        when nn $
            (input.buffer) %= init
        return ss
    | otherwise = do
        (input.buffer) %= (++[if shift m == Down then toUpper c else c])
        return ss 
handleMainEvent (EventKey (SpecialKey KeySpace) Down _ _) ss = do
    (input.buffer) %= (++[' '])
    return ss
handleMainEvent (EventKey (SpecialKey KeyShiftL) Down _ _) ss = return ss
handleMainEvent (EventKey (SpecialKey KeyShiftR) Down _ _) ss = return ss
handleMainEvent (EventKey (SpecialKey KeyUp) Down _ _) ss = do
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
handleMainEvent (EventKey (SpecialKey KeyEnter) Down _ _) ss = do
    code <- use (input.buffer)
    (input.history) %= (++[code])
    ihLen <- uses (input.history) length
    (input.historyPointer) .= ihLen
    (input.buffer) .= ""
    (input.search) .= Nothing
    executeResponses $ executeCommand code ss
handleMainEvent (EventResize (x,y)) ss = do
    (fx,fy) <- use $ settings.fontDimensions
    let nx = div x fx
    let ny = div y fy
    (settings.dimensions) .= (nx,ny)
    clearCanvas
    return ss
handleMainEvent (EventKey _ Up _ _) ss = return ss
handleMainEvent (EventKey (MouseButton _) _ _ _) ss = return ss
handleMainEvent (EventMotion _) ss = return ss
handleMainEvent (EventKey (SpecialKey KeyEsc) Down _ _) ss = openMainMenu ss
handleMainEvent e ss = do
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