{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

{-# LANGUAGE UndecidableInstances #-}
-- {-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE GADTs #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
module Types where

import Data.Typeable (Typeable)
import Data.Dynamic (Dynamic)
import Data.Data (Data, dataTypeOf, toConstr, constrIndex, maxConstrIndex)

import Control.Monad.Trans.State
--import Control.Monad.IO.Class (liftIO)

import Control.Lens hiding (at)


--import qualified Data.IntMap.Strict as I
import Data.IntMap.Strict (IntMap)
import Data.Set (Set)
-- import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)

-- * Tags, TODO: move these
-- | Tag type, all possible tags for a 'Component'
data Tag =
    Position
  | Velocity
  | DynamicBody
  | StaticBody
  | Keyboard
  | Sprite
  | Renderable
  deriving (Show, Read, Ord, Eq, Typeable, Data)


--------------------------------------------------------------------------------
-- * Components


class Typeable a => Component a where
    cTag :: a -> Tag
    cIndex :: a -> TagId
    cIndex c = tagIndex $ cTag c


-- | non-symbolic identifier for a 'Tag'
newtype TagId = TagId Int deriving (Ord, Eq)

-- | map with TagIds as key
type TagIdMap a = Map TagId a

-- | Must be an instance of 'Component' typeclass.
type SomeComponent = Dynamic

--data PositionData = PositionData (V2 Int)
--    deriving (Show, Read, Typeable)
--instance Component PositionData where
--    cTag _ = Position


--------------------------------------------------------------------------------

-- | from 'EntityId' to 'SomeComponent'
type Components = IntMap SomeComponent


-- | id for existing 'RegisteredComponent'
newtype ComponentId = ComponentId Int

-- | existing 'Component'
--data RegisteredComponent = RegisteredComponent {
--    _ownerId  :: !EntityId
--  , _compId   :: !ComponentId
--  , _compData :: !SomeComponent
--}



--------------------------------------------------------------------------------
-- * Entities

-- | id for existing 'Entity'
newtype EntityId = EntityId Int deriving (Ord, Eq)

-- use only IDs?
--data Entity = Entity {
--    _eId   :: !EntityId
--}


data Alias =
    NoAlias
  | ThePlayer
  deriving (Show, Read, Eq)


--------------------------------------------------------------------------------
-- * Entity managment


type Cesh = StateT EntityManager IO


data EntityManager = EntityManager {
    _entityCounter   :: !Int
  , _entitySet       :: !(Set EntityId) -- !(IntMap Entity)
  , _entityParents   :: !(Map EntityId [EntityId])
  , _entitiesUpdated :: ![EntityId]
  , _compsByType     :: !(Map TagId Components)
  -- ^ Components are saved by type and then by assosiated entity.
  , _systems         :: ![Cesh ()]
}

data EntityLocation = EntityLocation {
    _locationTag    :: !TagId
  , _locationEntity :: !EntityId
}

--------------------------------------------------------------------------------
-- * Internal?

-- For working only with type Tag
tagPlaceholder :: Tag
tagPlaceholder = error "tagPlaceholder :: Tag"

tagIndexMax :: TagId
tagIndexMax = TagId . maxConstrIndex $ dataTypeOf tagPlaceholder

tagIndex :: Tag -> TagId
tagIndex tag = TagId . constrIndex $ toConstr tag


--------------------------------------------------------------------------------
-- * Lens

$(makeLenses ''EntityManager)  
$(makeLenses ''EntityLocation)  
-- -- $(makeLenses ''RegisteredComponent)
-- -- $(makeLenses ''Entity)  

