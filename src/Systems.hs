{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
module Systems where


import Control.Monad.IO.Class (liftIO)
--import Control.Monad    (when, liftM, join)
import Control.Lens

import qualified Data.IntMap.Strict as I
import qualified Data.Map.Strict as M
import Data.Dynamic (toDyn, fromDyn)

import Types


-- probably not needed:
--newtype StorableSystem = StorableSystem {toAction :: Cesh ()}

class System s where
    -- | Dependencies for the System, these are 'Component's which the System takes as input
    deps :: s -> [TagId]

    -- | Outputs of the System, these are 'Component's which the System can modify
    outs :: s -> [TagId]

    -- | Internal? Convert the system to an action. Specifies how the System is called
    --  and results are handled.
    toAction :: s -> Cesh ()


-- | Registers a 'System', meaning it is now in use and will be run every frame.
-- The order in which the 'System's are run is specified by the order they are registered.
registerSystem :: System s => s -> Cesh ()
registerSystem sys = do
    let storableSystem = toAction sys
    systems %= (storableSystem :)


-- | Container in which the Components are stored and passed for Systems
type CList a = [a]


-- | Input system
newtype InputSystem o = InputSystem (IO o) 

instance HasComponents o => System (InputSystem o) where
    deps _ = []
    outs _ = extractTagIds (error "typeholder" :: o) 
    toAction (InputSystem sys) = do
        res <- liftIO sys
        -- TODO: save res
        return ()


-- | Output system
newtype OutputSystem d = OutputSystem (d -> IO ())

instance HasComponents d => System (OutputSystem (CList d)) where
    deps _ = extractTagIds (error "typeholder" :: d)
    outs _ = []
    toAction (OutputSystem sys) = do
        -- TODO: get inputs
        liftIO $ sys undefined


-- | Semipure system
-- Pure computation, but may also add or remove Components (or Entities?).
newtype SemipureSystem d o = SemipureSystem (d -> Cesh o)

instance (HasComponents d, HasComponents o) => System (SemipureSystem (CList d) (CList o)) where
    deps _ = extractTagIds (error "typeholder" :: d)
    outs _ = extractTagIds (error "typeholder" :: o) 
    toAction (SemipureSystem sys) = do
        -- TODO: get inputs
        res <- sys undefined
        -- TODO: save results
        return ()


-- | Pure system
-- Can only do pure computations and may not modify anything else than what is in the type signature.
newtype PureSystem d o = PureSystem (d -> o)

instance (HasComponents d, HasComponents o) => System (PureSystem (CList d) (CList o)) where
    deps _ = extractTagIds (error "typeholder" :: d)
    outs _ = extractTagIds (error "typeholder" :: o) 
    toAction (PureSystem sys) = do
        -- TODO: get inputs
        let res = sys undefined
        -- TODO: save results
        return ()



class HasComponents a where
    extractTagIds :: a -> [TagId]
    mapWithTagIds :: Monad m => (TagId -> b -> m c) -> a -> m c
    toSomeComponents :: a -> TagIdMap SomeComponent

--instance HasComponents () where
--    extractTagIds _ = []


-- Input system might output only one component, which is duplicated
-- | Sets the component to be duplicated to all entities which needs it
newtype DuplicateComp c = DuplicateComp c
instance Component c => HasComponents (DuplicateComp c) where
    extractTagIds _ = [cIndex (error "typeholder" :: c)]
    toSomeComponents (DuplicateComp singletype) = 
        M.singleton (head $ extractTagIds singletype) (toDyn singletype)

instance Component c => HasComponents [c] where
    extractTagIds _ = [cIndex (error "typeholder" :: c)]
    toSomeComponents singletype = 
        M.singleton (head $ extractTagIds singletype) (toDyn singletype)


instance (HasComponents a, HasComponents b) => HasComponents (a,b) where
    extractTagIds _ = extractTagIds (error "typeholder" :: a) ++
                      extractTagIds (error "typeholder" :: b)

instance (HasComponents a, HasComponents b, HasComponents c) => HasComponents (a,b,c) where
    extractTagIds _ = extractTagIds (error "typeholder" :: a) ++
                      extractTagIds (error "typeholder" :: b) ++
                      extractTagIds (error "typeholder" :: c)

instance (HasComponents a, HasComponents b, HasComponents c, HasComponents d) => HasComponents (a,b,c,d) where
    extractTagIds _ = extractTagIds (error "typeholder" :: a) ++
                      extractTagIds (error "typeholder" :: b) ++
                      extractTagIds (error "typeholder" :: c) ++
                      extractTagIds (error "typeholder" :: d)

instance (HasComponents a, HasComponents b, HasComponents c, HasComponents d, HasComponents e) => 
        HasComponents (a,b,c,d,e) where
    extractTagIds _ = extractTagIds (error "typeholder" :: a) ++
                      extractTagIds (error "typeholder" :: b) ++
                      extractTagIds (error "typeholder" :: c) ++
                      extractTagIds (error "typeholder" :: d) ++
                      extractTagIds (error "typeholder" :: e)



