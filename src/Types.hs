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
-- import Control.Monad.IO.Class (liftIO)

import Control.Lens hiding (at)


-- import qualified Data.IntMap.Strict as I
import Data.IntMap.Strict (IntMap)
import Data.Set (Set)
-- import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)

-- TODO: move these
-- * Tags
-- | Tag type, a symbolic name type for a 'Component'.
-- Currently should contain all possible tags.
-- TODO: Make this extensible with the help of 'TagId'
data Tag =
    Position
  | Velocity
  | DynamicBody
  | StaticBody
  | Keyboard
  | Sprite
  | Renderable
  deriving (Show, Read, Ord, Eq, Typeable, Data)


-- ------------------------------------------------------------------------------
-- * Components


-- | Component can be any type and are identified by a Tag
class Typeable a => Component a where
    -- | Component Tag, should be unique
    cTag :: a -> Tag

    -- | Converts Tag to TagId, used internally
    cIndex :: a -> TagId
    cIndex = tagIndex . cTag


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


-- ------------------------------------------------------------------------------


-- | existing 'Component'
--data RegisteredComponent = RegisteredComponent {
--    _ownerId  :: !EntityId
--  , _compId   :: !ComponentId
--  , _compData :: !SomeComponent
--}



-- ------------------------------------------------------------------------------
-- * Entities

-- | id for existing 'Entity'
newtype EntityId = EntityId Int deriving (Ord, Eq)

-- use only IDs?
--data Entity = Entity {
--    _eId   :: !EntityId
--}


-- TODO: not used yet
-- | Aliases for finding important single entities
data Alias =
    NoAlias
  | ThePlayer
  deriving (Show, Read, Eq)


-- ------------------------------------------------------------------------------
-- * Entity managment


-- | Monad stack for the component entity system framework
type Cesh = StateT EntityManager IO

-- | from 'EntityId' to 'SomeComponent'
type Components = IntMap SomeComponent

-- | id for existing 'RegisteredComponent'
newtype ComponentId = ComponentId Int


data EntityManager = EntityManager {
    _entityCounter   :: !Int

  -- TODO: what if we run out of integers?
  -- Should use https://github.com/SimSaladin/reusable-identifiers
  --, _availableEIds   :: !

  , _entitySet       :: !(Set EntityId) -- !(IntMap Entity)

  , _entitiesChangedV2 :: !(Set EntityId)
  -- ^ Tells which entities needs to be resolved in the end of a System

  -- Will be removed?
  -- , _entityParents   :: !(Map EntityId [EntityId])
  -- , _entitiesUpdated :: ![EntityId]

  , _compsByType     :: !(Map TagId Components)
  -- ^ Components are saved by type and then by assosiated entity.

  , _systems         :: ![Cesh ()]
  -- ^ Systems are just state changing actions at this level
}

data ComponentLocation = ComponentLocation {
    _locationTag    :: !TagId
  , _locationEntity :: !EntityId
}

-- ------------------------------------------------------------------------------
-- * Internal?

-- For working only with type Tag
tagPlaceholder :: Tag
tagPlaceholder = error "tagPlaceholder :: Tag"

tagIndexMax :: TagId
tagIndexMax = TagId . maxConstrIndex $ dataTypeOf tagPlaceholder

tagIndex :: Tag -> TagId
tagIndex tag = TagId . constrIndex $ toConstr tag


-- ------------------------------------------------------------------------------
-- * Lens

$(makeLenses ''EntityManager)
$(makeLenses ''ComponentLocation)
-- -- $(makeLenses ''RegisteredComponent)
-- -- $(makeLenses ''Entity)

