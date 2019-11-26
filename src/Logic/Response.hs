module Logic.Response where

import Logic.Dialogue

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State

data Response = TextResponse {
        _responseText :: String
    } | InventoryResponse | OpenMenuResponse {
        _responseMenuName :: String
    } | CloseMenuResponse | InitiateDialogueResponse {
        _responseDialogueTree :: DialogueTree
    }

makeFields ''Response

data Responding a = Responding {
    _respondingResponses :: [Response],
    _respondingResult :: a
}

makeFields ''Responding

instance Functor Responding where
    fmap f x = x >>= (return.f)

instance Applicative Responding where
    pure = return
    f <*> x = f >>= (<$>x)

instance Monad Responding where
    return = Responding []
    x >>= f = f(x^.result)&responses %~ ((x^.responses)++)

instance (Semigroup a) => Semigroup (Responding a) where
    r1 <> r2 = r1&responses %~ (++(r2^.responses))

instance (Monoid a) => Monoid (Responding a) where
    mempty = return mempty

respond :: Response -> StateT s Responding ()
respond r = responds [r]

responds :: [Response] -> StateT s Responding ()
responds rs = lift $ Responding rs ()

respondText :: String -> StateT s Responding ()
respondText = respond . TextResponse 