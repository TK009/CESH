
CESH - Entity Component System framework for Haskell
====================================================

Experimental implementation of [ECS](https://en.wikipedia.org/wiki/Entity_component_system) in haskell.
It tries to offer an easy syntax (EDSL) for defining Systems through haskell type system.

Project Status
--------------
In progress, not yet usable

Background
----------
ECSs are often composed from three things:

1. Entities
   
   Entities are usually just ids, but have Components associated to them so they
   can be seen as a bag of Components.

2. Components

   Components are any datatypes and all state and data of the application is contained in
   them.

3. Systems

   Systems are functions that take only Components of Entities matching their input-selector as input
   and outputs possibly modified Components. Systems can also add or remove Entities and
   Components.


