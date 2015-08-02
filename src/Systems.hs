{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
module Systems where


import Control.Monad.IO.Class (liftIO)
--import Control.Monad    (when, liftM, join)
import Control.Lens

import qualified Data.IntMap.Strict as I

import Types
import HasComponents


-- probably not needed:
--newtype StorableSystem = StorableSystem {toAction :: Cesh ()}

-- | Systems are run on every entity that matches the input matcher
class System s where
    -- | Dependencies for the System, these are 'Component's which the System takes as input
    deps :: s -> [TagId]

    -- | Outputs of the System, these are 'Component's which the System can modify
    outs :: s -> [TagId]

    -- | Internal; Convert the system to an action. Specifies how the System is called
    --  and results are handled.
    toAction :: s -> Cesh ()


-- | Registers a 'System', meaning it is now in use and will be run every frame.
-- The order in which the 'System's are run is specified by the order they are registered.
-- NOTE: This should be done before any Entities are added.
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
        entities <- _
        res <- liftIO sys
        compsByType %= (M.union $ toSomeComponents res)

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





