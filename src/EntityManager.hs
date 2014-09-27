module EntityManager where

import Control.Monad (liftM, forM, guard)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Lens
import Data.Monoid (mconcat)

import qualified Data.IntMap.Strict as I
import qualified Data.Map.Strict as M
import Data.IntMap.Strict (IntMap)
import Data.Map.Strict (Map)


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
resolveEntity :: EntityId -> [TagId] -> MaybeT Cesh [EntityLocation]
resolveEntity e tags = do
    entityOwns  <- lift entityMatch
    parentsOwns <- lift parentsMatch

    guard $ entityCheckTags (M.elems entityOwns) (M.elems parentsOwns)

    -- union discards duplicates in the second parameter
    let allMatches = entityOwns `M.union` parentsOwns

    -- now should hold:
    -- guard . and $ elems allMatches 
    return . map eLocation $ M.keys allMatches
    

  where
    eLocation tag = EntityLocation tag e
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



