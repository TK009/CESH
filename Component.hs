


data Component a = Component String a deriving(Show, Read, Eq)

type Class a = [Component a]

data Entity a = Entity {eId :: Int, eClass :: Class a}

type System = [Entity] -> [Entity]


buildSystem :: [Component a] -> -- ^ Inputs for the System
                System
buildSystem = id


