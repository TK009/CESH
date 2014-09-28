module EntityManager where

import Control.Monad (liftM, forM, guard, when)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Lens
import Data.Monoid (mconcat)

import qualified Data.IntMap.Strict as I
import Data.IntMap.Strict (IntMap)
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import qualified Data.Set as S


import Types

-- * Entities

-- ** Addition and Removal

-- | Only empty entities can be created. Components can then added to the created entity
createEntity :: Cesh EntityId
createEntity = do
    nextId <- use entityCounter
    entityCounter += 1

    let newId = EntityId nextId
    entitySet %= S.insert newId

    return newId

-- | Remove entity and associated components. Returns False if entity was not found.
removeEntity :: EntityId -> Cesh Bool
removeEntity e = do
    entityExists <- use $ entitySet . contains e
    --let entityExists = e `S.member` entities

    when entityExists $
        removeComponents e
    return entityExists

  where 
    -- remove any components associated to given entity
    removeComponents :: EntityId -> Cesh ()
    removeComponents (EntityId eid) = 
        compsByType . traverse %= (at eid .~ Nothing)

   
removeComponent :: ComponentLocation -> Cesh ()
removeComponent (ComponentLocation tag (EntityId eid)) =
    -- update 2D IntMap: update at tag, traverse Just value at eid setting it to Nothing which removes it
    compsByType . at tag %= (_Just . at eid .~ Nothing)
    

-- ** Checks and getters

entityMember :: EntityId -> IntMap a -> Bool
entityMember (EntityId eid) = I.member eid

entityLookup :: EntityId -> IntMap a -> Maybe a
entityLookup (EntityId eid) = I.lookup eid

{-
-- | This searches entity's components from all components
getEntityComponents :: EntityId -> Cesh [ComponentLocation]
getEntityComponents e = do
    allByType <- use compsByType


  where isMember = entityMember e
        -}


-- | Get parent(s), ancestors also?
getEntityParents :: EntityId -> Cesh [EntityId]
getEntityParents eid = do
    mParents <- use $ entityParents . at eid :: Cesh (Maybe [EntityId])

    case mParents of
        Just parents -> do
            ancestors <- concatMapM getEntityParents parents 
            return $ parents ++ ancestors
        Nothing -> return []

  where concatMapM f xs = liftM concat (mapM f xs)

-- | Returns list of Bools corresponding to given list of 'TagId's
entityHasTags :: EntityId -> [TagId] -> Cesh (Map TagId Bool)
entityHasTags e tags = do
    singletons <- forM tags $ \tag -> do
        bool <- entityHasTag e tag
        return $ M.singleton tag bool
    return $ mconcat singletons

entityHasTag :: EntityId -> TagId -> Cesh Bool
entityHasTag e tagId = do
    maybeTagComps <- use $ compsByType . at tagId

    return $ case maybeTagComps of
        Just tagComps -> e `entityMember` tagComps
        Nothing       -> False


-- | Tests whether entity contains all components given by '[TagId]',
-- some but not all can be from parents or ancestors, if so, results to asked components.
resolveEntity :: EntityId -> [TagId] -> MaybeT Cesh [ComponentLocation]
resolveEntity e tags = do
    entityOwns  <- lift entityMatch
    parentsOwns <- lift parentsMatch

    guard $ entityCheckTags (M.elems entityOwns) (M.elems parentsOwns)

    -- union discards duplicates in the second parameter
    let allMatches = entityOwns `M.union` parentsOwns

    -- now should hold:
    -- guard . and $ elems allMatches 
    return . map cLocation $ M.keys allMatches
    

  where
    cLocation tag = ComponentLocation tag e
    getParents    = getEntityParents e

    entityMatch  :: Cesh (Map TagId Bool)
    entityMatch  = entityHasTags e tags 

    parentsMatch :: Cesh (Map TagId Bool)
    parentsMatch = do
        parents <- getParents

        results <- forM parents $ \parent ->
            entityHasTags parent tags
        return $ mconcat results

    or2 = zipWith (||)

    entityCheckTags :: [Bool] -> [Bool] -> Bool
    entityCheckTags entityOwns parentsOwns =
            and entityOwns ||
               (or entityOwns &&
                and (entityOwns `or2` parentsOwns)
               )



