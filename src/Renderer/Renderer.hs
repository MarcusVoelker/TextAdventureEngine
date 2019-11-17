module Renderer.Renderer where

import Logic.GameState
import Logic.Response

import Engine

import System.Console.ANSI
import Control.Lens
import Control.Monad

data RendererState = RendererState {

}

makeFields ''RendererState

executeResponse :: GameState -> Response -> IO GameState
executeResponse gs (TextResponse s) = do
    putStrLn s 
    return gs

executeResponses :: Responding GameState -> IO GameState
executeResponses (Responding responses gs) = foldM executeResponse gs responses

renderEngine = Engine (do
    size <- getTerminalSize
    case size of
        Just (x,y) -> print (x,y)
        Nothing -> putStrLn "Unsupported Terminal!"
    clearScreen
    )
    clearScreen