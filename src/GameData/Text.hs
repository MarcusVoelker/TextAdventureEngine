module GameData.Text where

import qualified Data.Map as M


data VarCondition = CTrue | CIsTrue String | CNot VarCondition | CEqual String Int

data RenderDirection = Shake | Color Int Int Int

data VariadicLexeme = RawText String | VRenderText RenderDirection VariadicText | ConditionalText VarCondition VariadicText
type VariadicText = [VariadicLexeme]

data ResolvedLexeme = Text String | RenderText RenderDirection ResolvedText
type ResolvedText = [ResolvedLexeme]

evaluateCondition :: M.Map String Int -> VarCondition -> Bool
evaluateCondition _ CTrue = True
evaluateCondition m (CIsTrue v) = M.findWithDefault 0 v m > 0
evaluateCondition m (CNot c) = not $ evaluateCondition m c
evaluateCondition m (CEqual v i) = M.findWithDefault 0 v m == i

resolveText :: M.Map String Int -> VariadicText -> ResolvedText
resolveText m t = t >>= resolveLexeme m

resolveLexeme :: M.Map String Int -> VariadicLexeme -> ResolvedText
resolveLexeme _ (RawText s) = [Text s]
resolveLexeme m (VRenderText r v) = [RenderText r (resolveText m v)]
resolveLexeme m (ConditionalText c t) = if evaluateCondition m c then resolveText m t else []

liftString :: String -> ResolvedText
liftString = return . Text