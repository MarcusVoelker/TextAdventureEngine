module Frontend.Frontend where

import Frontend.Canvas
import Frontend.CanvasRenderer
import Frontend.Primitives
import Frontend.State
import Frontend.FrontState
import Frontend.Text
import Frontend.Window

import GameData.Text hiding (Color)

import Logic.Deserialiser
import Logic.Driver
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

renderMenu :: MenuState -> [ResolvedText]
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

openMainMenu :: StateStack -> FrontMod StateStack
openMainMenu ss = do
    let ss' = openMenuContext (MenuState mainMenu 0) ss
    openTopWindow 
        (Absolute 0)
        (Absolute 0) 
        (Absolute (-1)) 
        (Absolute (-1))
        (\st _ -> applyMenuFunction [liftString "Nobody here but us chickens!"] renderMenu st)
        (contextCount ss)
    return ss'

executeResponse :: StateStack -> Response -> FrontMod StateStack
executeResponse ss (TextResponse s) = do
    textHistory %= (++[s])
    return ss
executeResponse ss (OpenMenuResponse "main") = openMainMenu ss
executeResponse ss LeaveContextResponse = do
    closeContextWindows (contextCount ss - 1)
    return $ closeContext ss
executeResponse ss SaveResponse = lift (saveObject "save.dat" (ss^.globalGameState)) >> return ss
executeResponse ss LoadResponse = do
    forM_ [1..contextCount ss - 1] closeContextWindows
    dc <- use deserialisationContext
    textHistory .= [["Loaded Savegame"]]
    buildStateStack <$> lift (loadObject "save.dat" dc)
executeResponse _ QuitResponse = lift exitSuccess
executeResponse _ _ = lift $ throwIO $ PatternMatchFail "Unhandled Response!"

executeResponses :: RespondingFrontMod StateStack -> FrontMod StateStack
executeResponses rfm = do
    (Responding responses ss) <- runRespondingT rfm
    foldM executeResponse ss responses
    
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
handleEvent e ss = executeResponses $ stepStateStack (Stepper (handleMainEvent e) (handleMenuEvent e)) ss

handleMenuEvent :: Event -> MenuStepper RespondingFrontMod
handleMenuEvent (EventKey (SpecialKey KeyUp) Down _ _) = step1 $ \(MenuState menu idx) -> lift $ do
    let menuSize = length (menu^.content)
    return $ MenuState menu (mod (idx + menuSize - 1) menuSize)
handleMenuEvent (EventKey (SpecialKey KeyDown) Down _ _) = step1 $ \(MenuState menu idx) -> lift $ do
    let menuSize = length (menu^.content)
    return $ MenuState menu (mod (idx + 1) menuSize)
handleMenuEvent (EventKey (SpecialKey KeyEnter) Down _ _) = step1 $ \ms@(MenuState menu idx) -> do
    respondsT (((menu^.content) !! idx)^.content.action)
    return ms
handleMenuEvent (EventKey (SpecialKey KeyEsc) Down _ _) = step1 $ \ms -> do 
    respondT LeaveContextResponse
    return ms
handleMenuEvent (EventKey _ Up _ _) = step1 return
handleMenuEvent _ = step1 $ \st -> lift $ do
    lift $ putStrLn "Unhandled Menu State Event"
    return st

handleMainEvent :: Event -> GameStepper RespondingFrontMod
handleMainEvent (EventKey (Char c) Down m _) 
    | ord c == 8 = step1 $ \gs -> lift $ do
        nn <- uses (input.buffer) (not.null)
        when nn $
            (input.buffer) %= init
        return gs
    | otherwise = step1 $ \gs -> lift $ do
        (input.buffer) %= (++[if shift m == Down then toUpper c else c])
        return gs 
handleMainEvent (EventKey (SpecialKey KeySpace) Down _ _) = step1 $ \gs -> lift $ do
    (input.buffer) %= (++[' '])
    return gs
handleMainEvent (EventKey (SpecialKey KeyShiftL) Down _ _) = step1 return
handleMainEvent (EventKey (SpecialKey KeyShiftR) Down _ _)  = step1 return
handleMainEvent (EventKey (SpecialKey KeyUp) Down _ _) = step1 $ \gs -> lift $ do
    st <- use (input.search)
    when (isNothing st) $ do
        curCode <- use (input.buffer)
        (input.search) .= Just curCode
    searchTerm <- use (input.search)
    ip <- use (input.historyPointer)
    prefix <- uses (input.history) (reverse . take ip)
    case find (fromJust searchTerm `isPrefixOf`) prefix of
        Nothing -> return gs
        Just pCode -> do
            (input.historyPointer) .= fromJust (elemIndex pCode (reverse prefix))
            (input.buffer) .= pCode
            return gs
handleMainEvent (EventKey (SpecialKey KeyEnter) Down _ _) = \gs -> do
    code <- lift $ use (input.buffer)
    lift $ do
        (input.history) %= (++[code])
        ihLen <- uses (input.history) length
        (input.historyPointer) .= ihLen
        (input.buffer) .= ""
        (input.search) .= Nothing
    RespondingT $ return $ executeCommand code gs
handleMainEvent (EventResize (x,y)) = step1 $ \gs -> lift $ do
    (fx,fy) <- use $ settings.fontDimensions
    let nx = div x fx
    let ny = div y fy
    (settings.dimensions) .= (nx,ny)
    clearCanvas
    return gs
handleMainEvent (EventKey _ Up _ _) = step1 return
handleMainEvent (EventKey (MouseButton _) _ _ _) = step1 return
handleMainEvent (EventMotion _) = step1 return
handleMainEvent (EventKey (SpecialKey KeyEsc) Down _ _) = step1 $ \gs -> do
    respondT $ OpenMenuResponse "main"
    return gs
handleMainEvent e = step1 $ \gs -> lift $ do
    lift $ putStrLn $ "Unhandled Event " ++ show e
    return gs

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