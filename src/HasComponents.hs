{-# LANGUAGE ScopedTypeVariables #-}
module HasComponents where

-- | The whole purpose of this file is to give a nice syntax for defining 'System'
-- inputs and outputs: as tuples and some other features.


import Data.Dynamic (toDyn, fromDyn)
import qualified Data.Map.Strict as M

import Types




-- | Instances of this is supposed to be used as parameter (lists) of a System
class HasComponents a where
    -- | We need to transform the parameter list to our internal representation
    -- to find it from our datastructures
    extractTagIds :: a -> [TagId]

    -- TODO: wtf was this about
    -- mapWithTagIds :: Monad m => (TagId -> b -> m c) -> a -> m c

    -- | If System has return values, they need to be converted back to SomeComponents
    toSomeComponents   :: a -> TagIdMap SomeComponent
    fromSomeComponents :: TagIdMap SomeComponent -> a

-- Shouldn't be possible?
-- instance HasComponents () where
--     extractTagIds _ = []


-- TODO: wtf was this about?
-- | Future feature:
-- Input system might output only one component, which is duplicated
-- Sets the component to be duplicated to all entities which needs it
newtype DuplicateComp c = DuplicateComp c
instance Component c => HasComponents (DuplicateComp c) where
    extractTagIds _ = [cIndex (error "typeholder" :: c)]

    toSomeComponents (DuplicateComp singletype) =
        M.singleton (head $ extractTagIds [singletype]) (toDyn singletype)

    fromSomeComponents idmap =
        let tags       = extractTagIds (error "typeholder" :: DuplicateComp c)
            singletype :: SomeComponent
            singletype = idmap M.! head tags
            failure    = error "Internal type error"
        in DuplicateComp $ fromDyn singletype failure



-- | Future feature:
-- List type is used to seperated Systems that take one component at a time
-- and Systems that take all components as a list. (this being the latter case)
instance Component c => HasComponents [c] where
    extractTagIds    _ = [cIndex (error "typeholder" :: c)]
    toSomeComponents   = undefined
    fromSomeComponents = undefined
    {-

    toSomeComponents singletype =
        M.singleton (head $ extractTagIds singletype) (toDyn singletype)

    fromSomeComponents idmap =
        let tags       = extractTagIds (error "typeholder" :: [c])
            comps :: [SomeComponent]
            comps      = _ idmap
            failure    = error "Internal type error"
        in map (flip fromDyn failure) comps
        -}



instance (Component a, Component b) => HasComponents (a, b) where
    extractTagIds _ = [ cIndex (error "typeholder" :: a)
                      , cIndex (error "typeholder" :: b)
                      ]
    toSomeComponents   = undefined
    fromSomeComponents = undefined

instance (Component a, Component b, Component c) => HasComponents (a, b, c) where
    extractTagIds _ = [ cIndex (error "typeholder" :: a)
                      , cIndex (error "typeholder" :: b)
                      , cIndex (error "typeholder" :: c)
                      ]
    toSomeComponents   = undefined
    fromSomeComponents = undefined

instance (Component a, Component b, Component c, Component d) => HasComponents (a, b, c, d) where
    extractTagIds _ = [ cIndex (error "typeholder" :: a)
                      , cIndex (error "typeholder" :: b)
                      , cIndex (error "typeholder" :: c)
                      , cIndex (error "typeholder" :: d)
                      ]
    toSomeComponents   = undefined
    fromSomeComponents = undefined

instance (Component a, Component b, Component c, Component d, Component e) =>
        HasComponents (a, b, c, d, e) where
    extractTagIds _ = [ cIndex (error "typeholder" :: a)
                      , cIndex (error "typeholder" :: b)
                      , cIndex (error "typeholder" :: c)
                      , cIndex (error "typeholder" :: d)
                      , cIndex (error "typeholder" :: e)
                      ]
    toSomeComponents   = undefined
    fromSomeComponents = undefined


-- TODO: longer tuples

