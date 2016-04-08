{-# LANGUAGE
    KindSignatures
  , ConstraintKinds
  , MultiParamTypeClasses
  , FunctionalDependencies
  , FlexibleInstances
  , FlexibleContexts
  #-}

module Data.Unfoldable.Restricted where

import Data.Unfolder        (Unfolder (choose), ala, bfs)
import Data.Constraint      (Constraint)
import Data.Constraint.Unit (Unit)

import qualified Data.Set as Set
import qualified Data.Map as Map

import qualified Data.HashMap.Lazy as HashMap
import qualified Data.HashSet      as HashSet
import           Data.Hashable (Hashable)

import           Data.Maybe (maybeToList, isNothing)
import           Control.Monad.Trans.State


-- * Unfoldable

class UnfoldableR
        (pred :: * -> Constraint)
        (t :: * -> *)
      | t -> pred where
  unfoldRestrict :: (pred a, Unfolder f) => f a -> f (t a)

unfoldRestrict_ :: (UnfoldableR Unit t, Unfolder f) => f (t ())
unfoldRestrict_ = unfoldRestrict (pure ())

unfoldRestrictBF :: (UnfoldableR p t, Unfolder f, p a) => f a -> f (t a)
unfoldRestrictBF = ala bfs unfoldRestrict

unfoldRestrictBF_ :: (UnfoldableR Unit t, Unfolder f) => f (t ())
unfoldRestrictBF_ = bfs unfoldRestrict_

unfoldrRestrict :: (UnfoldableR p t, p a) => (b -> Maybe (a, b)) -> b -> Maybe (t a)
unfoldrRestrict f z = terminate . flip runStateT z . unfoldRestrictBF . StateT $ maybeToList . f
  where
    terminate [] = Nothing
    terminate ((t, b):ts) = if isNothing (f b) then Just t else terminate ts




class BiUnfoldableR
        (predA :: * -> Constraint)
        (predB :: * -> Constraint)
        (t :: * -> * -> *)
      | t -> predA predB where
  biunfoldRestrict :: (predA a, predB b, Unfolder f) => f a -> f b -> f (t a b)


-- Containers

instance UnfoldableR (Ord) Set.Set where
  unfoldRestrict fa = choose
    [ pure Set.empty
    , Set.singleton <$> fa
    , Set.union <$> unfoldRestrict fa <*> unfoldRestrict fa
    ]

instance BiUnfoldableR (Ord) Unit Map.Map where
  biunfoldRestrict fa fb = choose
    [ pure Map.empty
    , Map.singleton <$> fa <*> fb
    , Map.union <$> biunfoldRestrict fa fb <*> biunfoldRestrict fa fb
    ]


-- Unordered Containers


class (Hashable a, Eq a) => Hashable' a

instance UnfoldableR (Hashable') HashSet.HashSet where
  unfoldRestrict fa = choose
    [ pure HashSet.empty
    , HashSet.singleton <$> fa
    , HashSet.union <$> unfoldRestrict fa <*> unfoldRestrict fa
    ]

instance BiUnfoldableR (Hashable') (Unit) HashMap.HashMap where
  biunfoldRestrict fa fb = choose
    [ pure HashMap.empty
    , HashMap.singleton <$> fa <*> fb
    , HashMap.union <$> biunfoldRestrict fa fb <*> biunfoldRestrict fa fb
    ]
