module Frontend.Frontend where

import           Frontend.Canvas
import           Frontend.CanvasRenderer
import           Frontend.Primitives
import           Frontend.State
import           Frontend.FrontState
import           Frontend.Text
import           Frontend.Window

import           GameData.Text           hiding ( Color )

import           Logic.Deserialiser
import           Logic.Driver
import           Logic.Menu
import           Logic.Response
import           Logic.StateStack

import           Serialiser
import           Thing

import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Data.Char
import           Data.IORef
import           Data.List
import qualified Data.Map.Strict               as M
import           Data.Maybe
import           Graphics.UI.GLUT hiding (rect,Text,Color)
import           System.Exit

openTopWindow
    :: ScreenLoc
    -> ScreenLoc
    -> ScreenLoc
    -> ScreenLoc
    -> ContentView
    -> Int
    -> FrontMod ()
openTopWindow x y w h v c = do
    hand <- (+ 1) . maximum . M.keys <$> use windows
    let win = Window hand x y w h v c singleStyle
    windows %= M.insert hand win

closeContextWindows :: Int -> FrontMod ()
closeContextWindows c = windows %= M.filter (\w -> w ^. context /= c)

renderMenu :: MenuState -> [ResolvedText]
renderMenu (MenuState menu idx) =
    zip [ a == idx | a <- [0 ..] ] (menu ^. content)
        >>= (\case
                (False, c) ->
                    [ liftString ""
                    , Text " " : resolveText M.empty (c ^. name)
                    , liftString ""
                    ]
                (True, c) ->
                    let rt = resolveText M.empty (c ^. name)
                    in
                        [ liftString
                            (  bSTLCorner
                            :  (bSHLine <$ [1 .. textLength rt])
                            ++ [bSTRCorner]
                            )
                        , Text [bSVLine] : rt ++ [Text [bSVLine]]
                        , liftString
                            (  bSBLCorner
                            :  (bSHLine <$ [1 .. textLength rt])
                            ++ [bSBRCorner]
                            )
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
        (\st _ -> applyMenuFunction
            [liftString "Nobody here but us chickens!"]
            renderMenu
            st
        )
        (contextCount ss)
    return ss'

executeResponse :: StateStack -> Response -> FrontMod StateStack
executeResponse ss (TextResponse s) = do
    textHistory %= (++ [s])
    return ss
executeResponse ss (OpenMenuResponse "main") = openMainMenu ss
executeResponse ss LeaveContextResponse      = do
    closeContextWindows (contextCount ss - 1)
    return $ closeContext ss
executeResponse ss SaveResponse =
    lift (saveObject "save.dat" (ss ^. globalGameState)) >> return ss
executeResponse ss LoadResponse = do
    forM_ [1 .. contextCount ss - 1] closeContextWindows
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

initialFrontendState
    :: (Int, Int) -> (Int, Int) -> DeserialisationContext -> FrontendState
initialFrontendState (w, h) (fw, fh) = FrontendState
    []
    initialInputState
    initialCanvasState
    (M.fromList
        [ ( 0
          , Window 0
                   (Absolute 0)
                   (Absolute (-3))
                   (Absolute (-1))
                   (Absolute (-1))
                   (\_ fs -> [liftString $ "> " ++ (fs ^. input . buffer)])
                   0
                   doubleStyle
          )
        , ( 1
          , Window
              1
              (Absolute 0)
              (Absolute 0)
              (Absolute (-1))
              (Absolute (-4))
              (\_ fs ->
                  let (_, ch) = fs ^. settings . dimensions
                      th      = fs ^. textHistory
                  in  drop (max 0 (length th - ch + 5)) th
              )
              0
              singleStyle
          )
        ]
    )
    (FrontendSettings (w, h) (fw, fh))

{-
eventHandler :: Event -> IORef StateStack -> IORef FrontendState -> IO ()
eventHandler e ss fs = refLiftModStep ss fs (handleEvent e)

handleEvent :: Event -> StateStack -> FrontMod StateStack
handleEvent e ss = executeResponses
    $ stepStateStack (Stepper (handleMainEvent e) (handleMenuEvent e)) ss

handleMenuEvent :: Event -> MenuStepper RespondingFrontMod
handleMenuEvent (EventKey (SpecialKey KeyUp) Down _ _) =
    \(MenuState menu idx) -> lift $ do
        let menuSize = length (menu ^. content)
        return $ MenuState menu (mod (idx + menuSize - 1) menuSize)
handleMenuEvent (EventKey (SpecialKey KeyDown) Down _ _) =
    \(MenuState menu idx) -> lift $ do
        let menuSize = length (menu ^. content)
        return $ MenuState menu (mod (idx + 1) menuSize)
handleMenuEvent (EventKey (SpecialKey KeyEnter) Down _ _) =
    \ms@(MenuState menu idx) -> do
        respondsT (((menu ^. content) !! idx) ^. content . action)
        return ms
handleMenuEvent (EventKey (SpecialKey KeyEsc) Down _ _) = \ms -> do
    respondT LeaveContextResponse
    return ms
handleMenuEvent (EventKey _ Up _ _) = return
handleMenuEvent _                   = \st -> lift $ do
    lift $ putStrLn "Unhandled Menu State Event"
    return st

handleMainEvent :: Event -> GameStepper RespondingFrontMod
handleMainEvent (EventKey (Char c) Down m _)
    | ord c == 8 = \gs -> lift $ do
        nn <- uses (input . buffer) (not . null)
        when nn $ (input . buffer) %= init
        return gs
    | otherwise = \gs -> lift $ do
        (input . buffer) %= (++ [if shift m == Down then toUpper c else c])
        return gs
handleMainEvent (EventKey (SpecialKey KeySpace) Down _ _) = \gs -> lift $ do
    (input . buffer) %= (++ [' '])
    return gs
handleMainEvent (EventKey (SpecialKey KeyShiftL) Down _ _) = return
handleMainEvent (EventKey (SpecialKey KeyShiftR) Down _ _) = return
handleMainEvent (EventKey (SpecialKey KeyUp    ) Down _ _) = \gs -> lift $ do
    st <- use (input . search)
    when (isNothing st) $ do
        curCode <- use (input . buffer)
        (input . search) .= Just curCode
    searchTerm <- use (input . search)
    ip         <- use (input . historyPointer)
    prefix     <- uses (input . history) (reverse . take ip)
    case find (fromJust searchTerm `isPrefixOf`) prefix of
        Nothing    -> return gs
        Just pCode -> do
            (input . historyPointer)
                .= fromJust (elemIndex pCode (reverse prefix))
            (input . buffer) .= pCode
            return gs
handleMainEvent (EventKey (SpecialKey KeyEnter) Down _ _) = \gs -> do
    code <- lift $ use (input . buffer)
    lift $ do
        (input . history) %= (++ [code])
        ihLen <- uses (input . history) length
        (input . historyPointer) .= ihLen
        (input . buffer) .= ""
        (input . search) .= Nothing
    RespondingT $ return $ executeCommand code gs
handleMainEvent (EventResize (x, y)) = \gs -> lift $ do
    (fx, fy) <- use $ settings . fontDimensions
    let nx = div x fx
    let ny = div y fy
    (settings . dimensions) .= (nx, ny)
    clearCanvas
    return gs
handleMainEvent (EventKey _               Up _ _      ) = return
handleMainEvent (EventKey (MouseButton _) _  _ _      ) = return
handleMainEvent (EventMotion _                        ) = return
handleMainEvent (EventKey (SpecialKey KeyEsc) Down _ _) = \gs -> do
    respondT $ OpenMenuResponse "main"
    return gs
handleMainEvent e = \gs -> lift $ do
    lift $ putStrLn $ "Unhandled Event " ++ show e
    return gs
-}
updateHandler
    :: (StateStack, FrontendState) -> IO (StateStack, FrontendState)
updateHandler (ss, fs) = runStateT (stepFrontend ss) fs

stepFrontend :: StateStack -> FrontMod StateStack
stepFrontend ss = do
    wins <- M.elems <$> use windows
    mapM_ (renderWindow ss) wins
    return ss

idle :: IORef StateStack -> IORef FrontendState -> IdleCallback
idle ssr fsr = do
    ss <- readIORef ssr
    fs <- readIORef fsr
    (ss',fs') <- updateHandler (ss,fs)
    writeIORef ssr ss'
    writeIORef fsr fs'
    postRedisplay Nothing


renderHandler :: (StateStack, FrontendState) -> IO ()
renderHandler (_, fs) = runReaderT renderFrontend fs

display :: IORef StateStack -> IORef FrontendState -> DisplayCallback
display ssr fsr = do
    ss <- readIORef ssr
    fs <- readIORef fsr
    renderHandler (ss,fs)
    flush

{-screenEffect :: FrontRead (IO ())
screenEffect = do
    (cw, ch) <- views (settings . dimensions) (bimap fromIntegral fromIntegral)
    (fw, fh) <- views (settings . fontDimensions)
                      (bimap fromIntegral fromIntegral)
    let w = fw * cw
    let h = fh * ch
    return $ Translate (fw * (-cw / 2)) (fh * (ch / 2)) $ Pictures $ map
        (\y -> Color (makeColor 0 (fromIntegral (mod y 2)) 0 0.1)
            $ rect (0, -2 * fromIntegral y) (w, 2)
        )
        [0 .. div (round h - 1) 2]-}

renderFrontend :: FrontRead ()
renderFrontend = do
    --se <- screenEffect
    cv <- renderCanvas
    lift $ cv
