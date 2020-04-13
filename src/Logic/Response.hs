module Logic.Response where

import GameData.Text

import Control.Lens
import Control.Monad.Trans.Class
import Control.Monad.Trans.State

data Response = TextResponse {
        _responseText :: ResolvedText
    } | InventoryResponse | OpenMenuResponse {
        _responseMenuName :: String
    }  | LeaveContextResponse | SaveResponse | LoadResponse | QuitResponse

makeFields ''Response

data Responding a = Responding {
    _respondingResponses :: [Response],
    _respondingResult :: a
}

makeFields ''Responding

newtype RespondingT m a = RespondingT { runRespondingT :: m (Responding a) }

instance (Monad m) => Functor (RespondingT m) where
    fmap f x = x >>= return . f

instance (Monad m) => Applicative (RespondingT m) where
    pure = return
    f <*> x = f >>= (<$>x)

instance (Monad m) => Monad (RespondingT m) where
    return = RespondingT . return . Responding []
    x >>= f = RespondingT $ do
        res <- runRespondingT x 
        res' <- runRespondingT (f(res^.result))
        return $ res'&responses %~ ((res^.responses)++)

instance MonadTrans RespondingT where
    lift = RespondingT . fmap return

instance Functor Responding where
    fmap f x = x >>= return . f

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

respondT :: (Monad m) => Response -> RespondingT m ()
respondT r = RespondingT $ return $ Responding [r] ()

respond :: Response -> StateT s Responding ()
respond r = responds [r]

responds :: [Response] -> StateT s Responding ()
responds rs = lift $ Responding rs ()

respondText :: ResolvedText -> StateT s Responding ()
respondText = respond . TextResponse 

respondString :: String -> StateT s Responding ()
respondString = mapM_ (respondText . liftString) . lines
