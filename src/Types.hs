{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
module Types where

import Data.Typeable (Typeable)
import Data.Dynamic (Dynamic)
import Data.Data (Data, dataTypeOf, toConstr, constrIndex, maxConstrIndex)

import Control.Monad.Trans.State
import System.Random

import Control.Lens hiding (at)
import Linear

--import qualified Data.IntMap.Strict as Map
import Data.IntMap.Strict (IntMap)
import Data.Set (Set)
--import qualified Data.Map.Strict as SMap

-- * Tags?
-- | Tag type, all possible tags for a 'Component'
data Tag =
    Position
  | Velocity
  | DynamicBody
  | StaticBody
  | Keyboard
  | Sprite
  | Renderable deriving (Show, Ord, Eq, Typeable, Data)


--------------------------------------------------------------------------------
-- * Components


class Typeable a => Component a where
    cTag :: a -> Tag
    cIndex :: a -> CompTagId
    cIndex c = tagIndex $ cTag c
    --getData :: Typeable a => SomeComponent -> a
    --getData = fromDynamic


-- | non-symbolic identifier for a 'Tag'
type CompTagId = Int

-- | Must be an instance of 'Component' typeclass.
type SomeComponent = Dynamic


data PositionData = PositionData (V2 Int)
    deriving (Show, Read, Typeable)
instance Component PositionData where
    cTag _ = Position


--------------------------------------------------------------------------------

-- | from 'ComponentId' to 'SomeComponent'
type Components = IntMap SomeComponent
-- | id for existing 'Entity'
type EntityId = Int
-- | id for existing 'RegisteredComponent'
type ComponentId = Int

-- | existing 'Component'
data RegisteredComponent = RegisteredComponent {
    _ownerId  :: !EntityId
  , _compId   :: !ComponentId
  , _compData :: !SomeComponent
}

data Entity = Entity {
    _eId   :: !Int
  , _alias :: !Alias
}


data Alias =
    NoAlias
  | ThePlayer
  deriving (Show, Read, Eq)


--------------------------------------------------------------------------------
-- * Entity managment


type GameMonad a = StateT GameWorld IO a

data GameWorld = GameWorld {
    _random        :: !StdGen
  , _entityManager :: !EntityManager
}

data EntityManager = EntityManager {
    _entityCounter :: !Int
  , _entities      :: !(IntMap Entity)
  , _components    :: !Components
  , _systemCounter :: !Int
  , _systemGrouping:: !(IntMap (Set ComponentId)) -- ^ Contains the 'ComponentId''s with a SystemId as the key
  , _systems       :: !(IntMap RegisteredSystem)
}

-- | Systems should follow this type
type System a = a -> GameMonad a

-- | An active 'System'
data RegisteredSystem = RegisteredSystem {
    _sysInputs :: ![CompTagId]
  , _sysFunc   :: !(System Dynamic)
}

--------------------------------------------------------------------------------
-- * Internal?

tagIndexMax :: CompTagId
tagIndexMax = maxConstrIndex $ dataTypeOf (undefined :: Tag) 

tagIndex :: Tag -> CompTagId
tagIndex tag = constrIndex $ toConstr tag


--------------------------------------------------------------------------------
-- * Lens

$(makeLenses ''GameWorld)  
$(makeLenses ''EntityManager)  
$(makeLenses ''Entity)  
$(makeLenses ''RegisteredSystem)

