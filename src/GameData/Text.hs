module GameData.Text where

import qualified Data.Map as M
import Data.String

import Serialiser

data VarCondition = CTrue | CIsTrue String | CNot VarCondition | CEqual String Int

data RenderDirection = Shake | Color Int Int Int

data MetaLexeme = RawText String | MRenderText RenderDirection MetaText | ConditionalText VarCondition MetaText
type MetaText = [MetaLexeme]

instance Serialisable VarCondition where
    serialise CTrue = serialise 't'
    serialise (CIsTrue s) = serialise '?' <> serialise s
    serialise (CNot c) = serialise '!' <> serialise c
    serialise (CEqual s i) = serialise '=' <> serialise s <> serialise i

instance Persistent VarCondition ctx where
    deserialise = do
        c <- deserialise 
        case c of 
            't' -> return CTrue
            '?' -> CIsTrue <$> deserialise
            '!' -> CNot <$> deserialise
            '=' -> CEqual <$> deserialise <*> deserialise

instance Serialisable RenderDirection where
    serialise Shake = serialise 's'
    serialise (Color r g b) = serialise 'c' <> serialise r <> serialise g <> serialise b

instance Persistent RenderDirection ctx where
    deserialise = do
        c <- deserialise
        case c of
            's' -> return Shake
            'c' -> Color <$> deserialise <*> deserialise <*> deserialise

instance Serialisable MetaLexeme where
    serialise (RawText s) = serialise 's' <> serialise s
    serialise (MRenderText d t) = serialise 'r' <> serialise d <> serialise t
    serialise (ConditionalText c t) = serialise 'c' <> serialise c <> serialise t

instance Persistent MetaLexeme ctx where
    deserialise = do
        c <- deserialise
        case c of
            's' -> RawText <$> deserialise
            'r' -> MRenderText <$> deserialise <*> deserialise
            'c' -> ConditionalText <$> deserialise <*> deserialise


data ResolvedLexeme = Text String | RenderText RenderDirection ResolvedText
type ResolvedText = [ResolvedLexeme]

evaluateCondition :: M.Map String Int -> VarCondition -> Bool
evaluateCondition _ CTrue = True
evaluateCondition m (CIsTrue v) = M.findWithDefault 0 v m > 0
evaluateCondition m (CNot c) = not $ evaluateCondition m c
evaluateCondition m (CEqual v i) = M.findWithDefault 0 v m == i

resolveText :: M.Map String Int -> MetaText -> ResolvedText
resolveText m t = t >>= resolveLexeme m

resolveLexeme :: M.Map String Int -> MetaLexeme -> ResolvedText
resolveLexeme _ (RawText s) = [Text s]
resolveLexeme m (MRenderText r v) = [RenderText r (resolveText m v)]
resolveLexeme m (ConditionalText c t) = if evaluateCondition m c then resolveText m t else []

liftString :: String -> ResolvedText
liftString = return . Text

textLength :: ResolvedText -> Int
textLength = sum . map (\case
    Text s -> length s
    RenderText _ rt -> textLength rt
    )

instance IsString MetaLexeme where
    fromString = RawText

instance IsString ResolvedLexeme where
    fromString = Text