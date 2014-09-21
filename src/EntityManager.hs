module EntityManager where

import Control.Monad (liftM, forM, guard)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Lens

import qualified Data.IntMap.Strict as I
import Data.IntMap.Strict (IntMap)


import Types


entityMember :: EntityId -> IntMap a -> Bool
entityMember (EntityId eid) = I.member eid

entityLookup :: EntityId -> IntMap a -> Maybe a
entityLookup (EntityId eid) = I.lookup eid

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
entityHasTags :: EntityId -> [TagId] -> Cesh [Bool]
entityHasTags e = mapM (entityHasTag e)

entityHasTag :: EntityId -> TagId -> Cesh Bool
entityHasTag e tagId = do
    maybeTagComps <- use $ compsByType . at tagId

    return $ case maybeTagComps of
        Just tagComps -> e `entityMember` tagComps
        Nothing       -> False

-- # Hierarchies

-- | Tests whether entity contains all components given by '[TagId]',
-- some but not all can be from parents or ancestors, if so, results to asked components.
resolveEntity :: EntityId -> [TagId] -> MaybeT Cesh [SomeComponent]
resolveEntity e tags = do
    entityOwns  <- lift entityMatch
    parentsOwns <- lift parentsMatch

    guard $ entityCheckTags entityOwns parentsOwns

    
    
    return _

  where
    getParents = getEntityParents e

    entityMatch  :: Cesh [Bool]
    entityMatch  = entityHasTags e tags 

    parentsMatch :: Cesh [[Bool]]
    parentsMatch = do
        parents <- getParents

        forM parents $ \parent ->
            entityHasTags parent tags

    or2 = zipWith (||)
    orN = foldr1 or2 

    entityCheckTags entityOwns parentsOwns =
            and entityOwns ||
               (or entityOwns &&
                and (entityOwns `or2` orN parentsOwns)
               )



