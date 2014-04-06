{-# LANGUAGE ScopedTypeVariables #-}
module Systems where


import Control.Monad (liftM)
import Data.Dynamic (Typeable, Dynamic, toDyn, fromDyn)
import Control.Lens hiding (at)

import Data.IntMap.Strict (empty, insert)

import Types


{-
registerSystem2 :: forall a b. (Component a, Component b) =>
    System (a,b) -> GameMonad ()
registerSystem2 system = do
    nextId <- use $ entityManager.systemCounter
    entityManager.systemCounter += 1

    -- Make a group for the system
    let inputs = [indexA, indexB]
    entityManager.systemGrouping %= insert nextId inputs


    -- Add the system to the manager
    let dynamicSystem :: System Dynamic
        dynamicSystem x = liftM toDyn $ system $ fromDyn x typeError
    entityManager.systems %= insert nextId dynamicSystem
    
  where indexA = cIndex (undefined :: a)
        indexB = cIndex (undefined :: b)
        typeError = error "TypeError in systemGrouping, shouldn't happen!."
        -}


class Typeable x => HasComponents x where
    inputTags :: x -> [CompIndex]

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
    entityManager.systemCounter += 1
    nextId <- use $ entityManager.systemCounter

    -- Make a group for the system
    -- TODO this destroys registered Entities
    entityManager.systemGrouping %= insert nextId empty


    -- Add the system to the manager
    let inputs = inputTags (undefined :: s)
        dynamicSystem :: System Dynamic
        dynamicSystem x = liftM toDyn $ system $ fromDyn x typeError
    entityManager.systems %= insert nextId (RegisteredSystem inputs dynamicSystem)
    
  where typeError = error "TypeError in systemGrouping, shouldn't happen!." 


runSystems :: GameMonad ()
runSystems = --do
    --let inputs = mapWithKey entityManager.systemGrouping
    undefined


