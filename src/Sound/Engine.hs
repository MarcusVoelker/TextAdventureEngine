module Sound.Engine where

import Control.Monad
import Sound.ProteaAudio

import Engine

soundEngine :: Engine
soundEngine = Engine (do
        result <- initAudio 64 44100 1024
        unless result $ fail "Failed to initialise Sound Engine")
    finishAudio