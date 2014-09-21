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
    outs _ = extractTagIds (undefined :: o) 
    toAction (InputSystem sys) = do
        res <- liftIO sys
        -- TODO: save res
        return ()


-- | Output system
newtype OutputSystem d = OutputSystem (d -> IO ())

instance HasComponents d => System (OutputSystem (CList d)) where
    deps _ = extractTagIds (undefined :: d)
    outs _ = []
    toAction (OutputSystem sys) = do
        -- TODO: get inputs
        liftIO $ sys undefined


-- | Semipure system
-- Pure computation, but may also add or remove Components (or Entities?).
newtype SemipureSystem d o = SemipureSystem (d -> Cesh o)

instance (HasComponents d, HasComponents o) => System (SemipureSystem (CList d) (CList o)) where
    deps _ = extractTagIds (undefined :: d)
    outs _ = extractTagIds (undefined :: o) 
    toAction (SemipureSystem sys) = do
        -- TODO: get inputs
        res <- sys undefined
        -- TODO: save results
        return ()


-- | Pure system
-- Can only do pure computations and may not modify anything else than what is in the type signature.
newtype PureSystem d o = PureSystem (d -> o)

instance (HasComponents d, HasComponents o) => System (PureSystem (CList d) (CList o)) where
    deps _ = extractTagIds (undefined :: d)
    outs _ = extractTagIds (undefined :: o) 
    toAction (PureSystem sys) = do
        -- TODO: get inputs
        let res = sys undefined
        -- TODO: save results
        return ()



class HasComponents a where
    extractTagIds :: a -> [TagId]
    toSomeComponents :: a -> TagIdMap SomeComponent

--instance HasComponents () where
--    extractTagIds _ = []


instance Component c => HasComponents [c] where
    extractTagIds _ = [cIndex (undefined :: c)]
    toSomeComponents singletype = 
        M.singleton (head $ extractTagIds singletype) (toDyn singletype)


instance (HasComponents a, HasComponents b) => HasComponents (a,b) where
    extractTagIds _ = extractTagIds (undefined :: a) ++
                      extractTagIds (undefined :: b)

instance (HasComponents a, HasComponents b, HasComponents c) => HasComponents (a,b,c) where
    extractTagIds _ = extractTagIds (undefined :: a) ++
                      extractTagIds (undefined :: b) ++
                      extractTagIds (undefined :: c)

instance (HasComponents a, HasComponents b, HasComponents c, HasComponents d) => HasComponents (a,b,c,d) where
    extractTagIds _ = extractTagIds (undefined :: a) ++
                      extractTagIds (undefined :: b) ++
                      extractTagIds (undefined :: c) ++
                      extractTagIds (undefined :: d)

instance (HasComponents a, HasComponents b, HasComponents c, HasComponents d, HasComponents e) => 
        HasComponents (a,b,c,d,e) where
    extractTagIds _ = extractTagIds (undefined :: a) ++
                      extractTagIds (undefined :: b) ++
                      extractTagIds (undefined :: c) ++
                      extractTagIds (undefined :: d) ++
                      extractTagIds (undefined :: e)



