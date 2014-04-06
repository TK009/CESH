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

import qualified Data.IntMap.Strict as Map
--import qualified Data.Map.Strict as SMap

data Tag =
    Position
  | Velocity
  | DynamicBody
  | StaticBody
  | Keyboard
  | Sprite
  | Renderable deriving (Show, Ord, Eq, Typeable, Data)


--------------------------------------------------------------------------------


class Typeable a => Component a where
    cTag :: a -> Tag
    cIndex :: a -> CompIndex
    cIndex c = tagIndex $ cTag c
    --getData :: Typeable a => SomeComponent -> a
    --getData = fromDynamic

type CompIndex = Int
type SomeComponent = Dynamic


data PositionData = PositionData (V2 Int)
    deriving (Show, Read, Typeable)
instance Component PositionData where
    cTag _ = Position


--------------------------------------------------------------------------------

type Components = Map.IntMap SomeComponent -- EntityId to Somecomponent

data Entity = Entity {
    _eId :: !Int
  , _alias :: !Alias
}


data Alias =
    NoAlias
  | ThePlayer
  deriving (Show, Read, Eq)

type System a = a -> GameMonad a

--------------------------------------------------------------------------------

type GameMonad a = StateT GameWorld IO a

data GameWorld = GameWorld {
    _random :: StdGen
  , _entityManager :: EntityManager
}

data EntityManager = EntityManager {
    _entityCounter :: !Int
  , _entities      :: !(Map.IntMap Entity)
  --, _components    :: !Components -- TODO
  , _systemCounter :: !Int
  , _systemGrouping:: !(Map.IntMap Components)
  , _systems       :: !(Map.IntMap RegisteredSystem)
}

data RegisteredSystem = RegisteredSystem {
    _sysInputs :: [CompIndex]
  , _sysFunc   :: System Dynamic
}

--------------------------------------------------------------------------------
-- | Internal?

tagIndexMax :: CompIndex
tagIndexMax = maxConstrIndex $ dataTypeOf (undefined :: Tag) 

tagIndex :: Tag -> CompIndex
tagIndex tag = constrIndex $ toConstr tag


--------------------------------------------------------------------------------
$(makeLenses ''GameWorld)  
$(makeLenses ''EntityManager)  
$(makeLenses ''Entity)  
$(makeLenses ''RegisteredSystem)

