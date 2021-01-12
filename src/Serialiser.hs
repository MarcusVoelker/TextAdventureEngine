module Serialiser where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B 
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy.UTF8 as U

import Control.Monad
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Reader

import qualified Data.Map.Strict as M
import Data.Bits
import Data.Int
import Data.Word

class Serialisable a where
    serialise :: a -> Builder

class (Serialisable a) => Persistent a ctx where
    deserialise :: StateT ByteString (ReaderT ctx Maybe) a

extractWord :: StateT ByteString (ReaderT ctx Maybe) Word8
extractWord = do
    bs <- get
    case B.uncons bs of
        Just (w,r) -> do
            put r
            return w
        Nothing -> fail "Failure"

instance Serialisable Int where
    serialise = int64LE . fromIntegral
    
instance Persistent Int ctx where
    deserialise = do
        ws <- sequence (((fromIntegral :: Word8 -> Word64) <$> extractWord) <$ [1..8])
        return $ fromIntegral $ (fromIntegral :: Word64 -> Int64) $ foldr (\w r -> shiftL r 8 .|. w) 0 ws 

instance Serialisable Char where
    serialise = charUtf8

instance Persistent Char ctx where
    deserialise = do
        bs <- get
        case U.uncons bs of
            Just (c,r) -> do
                put r
                return c
            Nothing -> fail "Failure"


instance (Serialisable a, Serialisable b) => Serialisable (a,b) where
    serialise (a,b) = serialise a <> serialise b

instance (Persistent a ctx, Persistent b ctx) => Persistent (a,b) ctx where
    deserialise = (,) <$> deserialise <*> deserialise

instance (Serialisable a) => Serialisable [a] where
    serialise xs = serialise (length xs) <> mconcat (map serialise xs)

instance (Persistent a ctx) => Persistent [a] ctx where
    deserialise = do
        s <- deserialise :: StateT ByteString (ReaderT ctx Maybe) Int
        forM [1..s] $ const deserialise

instance (Ord k, Serialisable k , Serialisable a) => Serialisable (M.Map k a) where
    serialise m = serialise (M.size m) <> M.foldMapWithKey (\k a -> serialise k <> serialise a) m

instance (Ord k, Persistent k ctx, Persistent a ctx) => Persistent (M.Map k a) ctx where
    deserialise = do
        s <- deserialise :: StateT ByteString (ReaderT ctx Maybe) Int
        l <- forM [1..s] $ \_ -> do
            key <- deserialise
            val <- deserialise
            return (key,val)
        return $ M.fromList l

saveObject :: (Serialisable a) => String -> a -> IO ()
saveObject path o = B.writeFile path (toLazyByteString $ serialise o)

loadObject :: (Persistent a ctx) => String -> ctx -> IO a
loadObject path ctx = do
    bs <- B.readFile path
    case runReaderT (evalStateT deserialise bs) ctx of
        Nothing -> fail "Couldn't parse"
        Just res -> return res