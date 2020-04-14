module Thing where

import Control.Lens

class HasName s a | s -> a where
    name :: Lens' s a

class HasDescription s a | s -> a where
    description :: Lens' s a

class HasDisplayName s a | s -> a where
    displayName :: Lens' s a
    
class HasIdt s a | s -> a where
    idt :: Lens' s a

class HasLocation s a | s -> a where
    location :: Lens' s a

class HasContent s a | s -> a where
    content :: Lens' s a