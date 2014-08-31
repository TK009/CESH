{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Systems where


import Control.Monad    (liftM, join)
import Data.Maybe       (isJust,listToMaybe)
import Data.Dynamic     (Typeable, Dynamic, toDyn, fromDyn, fromDynamic)
import Control.Lens hiding (at)
import Data.List        (foldl')

import Data.IntMap.Strict (insert, IntMap) -- TODO switch to qualified names
import qualified Data.IntMap.Strict as I
import Data.Set (Set)
import qualified Data.Set as S

import Types


-- | Used to have 'System's that have input as tuples (or other types?)
class Typeable x => HasComponents x where
    inputTags :: x -> [CompTagId]

instance forall a. (Component a) => HasComponents (a) where
    inputTags _ = [ cIndex (undefined :: a) ]

instance forall a b. (Component a, Component b) => HasComponents (a,b) where
    inputTags _ = [ cIndex (undefined :: a)
                  , cIndex (undefined :: b)
                  ]
instance forall a b c. (Component a, Component b, Component c) => HasComponents (a,b,c) where
    inputTags _ = [ cIndex (undefined :: a)
                  , cIndex (undefined :: b)
                  , cIndex (undefined :: c)
                  ]
instance forall a b c d. (Component a, Component b, Component c, Component d) => HasComponents (a,b,c,d) where
    inputTags _ = [ cIndex (undefined :: a)
                  , cIndex (undefined :: b)
                  , cIndex (undefined :: c)
                  , cIndex (undefined :: d)
                  ]
instance forall a b c d e. (Component a, Component b, Component c, Component d, Component e) => HasComponents (a,b,c,d,e) where
    inputTags _ = [ cIndex (undefined :: a)
                  , cIndex (undefined :: b)
                  , cIndex (undefined :: c)
                  , cIndex (undefined :: d)
                  , cIndex (undefined :: e)
                  ]


registerSystem :: forall s. HasComponents s => System s -> GameMonad ()
registerSystem system = do
    eManager.systemCounter += 1
    nextId <- use $ eManager.systemCounter

    -- Make a group for the system
    -- TODO: handle inserting of entities
    eManager.systemGrouping %= insert nextId S.empty 


    -- Add the system to the manager
    let inputs = inputTags (undefined :: s)
        dynamicSystem :: System Dynamic
        dynamicSystem x = liftM toDyn $ system $ fromDyn x typeError
    eManager.systems %= insert nextId (RegisteredSystem inputs dynamicSystem)
    
  where typeError = error "TypeError in systemGrouping, shouldn't happen!." 


-- TODO: move to HasComponents typeclass?
-- | Convert a list of SomeComponent to a Dynamic tuple
toTupleDyn :: [SomeComponent] -> Dynamic
toTupleDyn [a]          = toDyn  a
toTupleDyn [a,b]        = toDyn (a,b)
toTupleDyn [a,b,c]      = toDyn (a,b,c)
toTupleDyn [a,b,c,d]    = toDyn (a,b,c,d)
toTupleDyn [a,b,c,d,e]  = toDyn (a,b,c,d,e)
toTupleDyn _ = error "toTupleDyn: Not implemented for this amount of components"

-- | Convert a Dynamic tuple to a list of SomeComponent
fromTupleDyn :: Dynamic -> [SomeComponent]
fromTupleDyn x = maybe typeError id . join . listToMaybe . filter isJust $
                 [ fmap tupleToList (fromDynamic x :: Maybe (SomeComponent,SomeComponent,SomeComponent,SomeComponent,SomeComponent))
                 , fmap tupleToList (fromDynamic x :: Maybe (SomeComponent,SomeComponent,SomeComponent,SomeComponent))
                 , fmap tupleToList (fromDynamic x :: Maybe (SomeComponent,SomeComponent,SomeComponent))
                 , fmap tupleToList (fromDynamic x :: Maybe (SomeComponent,SomeComponent))
                 , fmap (:[])       (fromDynamic x :: Maybe (SomeComponent))
                 ]
  where typeError = error "TypeError: couldn't convert result tuple to a list"
        tupleToList t = t^..each

keyErr :: a
keyErr = error "invalid key" -- shouldn't happen

runSystems :: GameMonad ()
runSystems = do
    systemMap  <- (use $ eManager.systems) :: GameMonad (IntMap RegisteredSystem)
    cgroupMap  <- (use $ eManager.systemGrouping) :: GameMonad (IntMap (Set [ComponentId]))
    curComponents <- (use $ eManager.components) :: GameMonad Components
    --allComponents <- liftM I.elems (use $ eManager.components)

    let entityMatches  = I.elems cgroupMap
        componentIdLists :: [[[ComponentId]]] -- Hierarchy: Systems, Entities, Components
        componentIdLists = map S.elems entityMatches
    allTargetGroups <- mapM (mapM getComps) componentIdLists

    let 
        allSystems = I.elems systemMap
        newComponents :: [[[SomeComponent]]]
        newComponents = do
            ((RegisteredSystem inputTagIds dynSystem), targetGroup) <- allSystems `zip` allTargetGroups
            return $ mapM (fromTupleDyn . dynSystem . toTupleDyn) targetGroup

    
    let keyValuePairs :: [(ComponentId,SomeComponent)]
        keyValuePairs = map _ componentIdLists `zip` newComponents
        updatedComponents = foldl' (\(key, c) cs -> insert key c cs) curComponents 

    eManager.components.= updatedComponents
    

-- | Get a set of components by their identifer numbers.
-- components for the given identifiers should always exist
getComps :: [ComponentId] -> GameMonad [SomeComponent]
getComps s = do
    comps <- use $ eManager.components
    let keys  = S.elems s
        getComponent k = I.findWithDefault keyErr k comps
    return $ map getComponent keys


