module Engine where

import Control.Exception
import Data.Monoid

data Engine = Engine { setup :: IO (), teardown :: IO ()}

instance Semigroup Engine where
    a <> b = Engine (setup a >> setup b) (teardown b >> teardown a)

withEngine :: Engine -> IO () -> IO ()
withEngine e c = setup e >> finally c (teardown e)