{-# LANGUAGE
    KindSignatures
  , ConstraintKinds
  , MultiParamTypeClasses
  , FunctionalDependencies
  , FlexibleInstances
  , FlexibleContexts
  , UndecidableInstances
  #-}

module Data.Unfoldable.Restricted where

import Data.Unfolder
import Data.Constraint      (Constraint)
import Data.Constraint.Unit (Unit)

import qualified Data.Set as Set
import qualified Data.Map as Map

import qualified Data.HashMap.Lazy as HashMap
import qualified Data.HashSet      as HashSet
import           Data.Hashable (Hashable)

import           Data.Maybe (maybeToList, isNothing)
import           Data.Functor.Identity
import           Data.Functor.Constant
import           Data.Functor.Product
import           Data.Functor.Sum
import           Data.Functor.Reverse
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
    terminate []          = Nothing
    terminate ((t, b):ts) = if isNothing (f b) then Just t else terminate ts

fromList :: (UnfoldableR p t, p a) => [a] -> Maybe (t a)
fromList = unfoldrRestrict uncons
  where
    uncons []     = Nothing
    uncons (a:as) = Just (a, as)

leftMost :: (UnfoldableR Unit t) => Maybe (t ())
leftMost = unfoldRestrict_

rightMost :: (UnfoldableR Unit t) => Maybe (t ())
rightMost = getDualA unfoldRestrict_

allDepthFirst :: (UnfoldableR Unit t) => [t ()]
allDepthFirst = unfoldRestrict_

allToDepth :: (UnfoldableR Unit t) => Int -> [t ()]
allToDepth d = limitDepth d unfoldRestrict_

allBreadthFirst :: (UnfoldableR Unit t) => [t ()]
allBreadthFirst = unfoldRestrictBF_


-- * BiUnfoldable

class BiUnfoldableR
        (predA :: * -> Constraint)
        (predB :: * -> Constraint)
        (t :: * -> * -> *)
      | t -> predA predB where
  biunfoldRestrict :: (predA a, predB b, Unfolder f) => f a -> f b -> f (t a b)

biunfoldRestrict_ :: (BiUnfoldableR Unit Unit t, Unfolder f) => f (t () ())
biunfoldRestrict_ = biunfoldRestrict (pure ()) (pure ())

biunfoldRestrictBF :: (BiUnfoldableR p q t, Unfolder f, p a, q b) => f a -> f b -> f (t a b)
biunfoldRestrictBF = ala2 bfs biunfoldRestrict

biunfoldRestrictBF_ :: (BiUnfoldableR Unit Unit t, Unfolder f) => f (t () ())
biunfoldRestrictBF_ = bfs biunfoldRestrict_

biunfoldrRestrict :: ( BiUnfoldableR p q t
                     , p a
                     , q b
                     ) => (c -> Maybe (a, c))
                       -> (c -> Maybe (b, c))
                       -> c -> Maybe (t a b)
biunfoldrRestrict fa fb z = terminate . flip runStateT z $
  biunfoldRestrictBF (StateT $ maybeToList . fa) (StateT $ maybeToList . fb)
  where
    terminate []          = Nothing
    terminate ((t, c):ts) = if isNothing (fa c) && isNothing (fb c)
                            then Just t else terminate ts

fromLists :: (BiUnfoldableR p q t, p a, q b) => [a] -> [b] -> Maybe (t a b)
fromLists = curry $ biunfoldrRestrict unconsA unconsB
  where
    unconsA ([],_)     = Nothing
    unconsA (a:as, bs) = Just (a, (as, bs))
    unconsB (_,[])     = Nothing
    unconsB (as, b:bs) = Just (b, (as, bs))

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


-- * Utils


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

-- Orig

instance UnfoldableR Unit [] where
  unfoldRestrict fa = choose
    [ pure []
    , (:) <$> fa <*> unfoldRestrict fa
    ]

instance UnfoldableR Unit Maybe where
  unfoldRestrict fa = choose
    [ pure Nothing
    , Just <$> fa
    ]

instance ( Bounded a
         , Enum a
         ) => UnfoldableR Unit (Either a) where
  unfoldRestrict fa = choose
    [ Left <$> boundedEnum
    , Right <$> fa
    ]

instance ( Bounded a
         , Enum a
         ) => UnfoldableR Unit ((,) a) where
  unfoldRestrict fa = choose
    [ (,) <$> boundedEnum <*> fa
    ]

instance UnfoldableR Unit Identity where
  unfoldRestrict fa = choose
    [ Identity <$> fa
    ]

instance ( Bounded a
         , Enum a
         ) => UnfoldableR Unit (Constant a) where
  unfoldRestrict _ = choose
    [ Constant <$> boundedEnum
    ]

instance ( UnfoldableR p f
         , UnfoldableR p g
         ) => UnfoldableR p (Product f g) where
  unfoldRestrict fa = choose
    [ Pair <$> unfoldRestrict fa <*> unfoldRestrict fa
    ]

instance ( UnfoldableR p f
         , UnfoldableR p g
         ) => UnfoldableR p (Sum f g) where
  unfoldRestrict fa = choose
    [ InL <$> unfoldRestrict fa
    , InR <$> unfoldRestrict fa
    ]

instance UnfoldableR p f => UnfoldableR p (Reverse f) where
  unfoldRestrict fa = choose
    [ Reverse <$> getDualA (unfoldRestrict (DualA fa))
    ]


instance BiUnfoldableR Unit Unit Either where
  biunfoldRestrict fa fb = choose
    [ Left <$> fa
    , Right <$> fb
    ]

instance BiUnfoldableR Unit Unit (,) where
  biunfoldRestrict fa fb = choose
    [ (,) <$> fa <*> fb
    ]

instance BiUnfoldableR Unit Unit Constant where
  biunfoldRestrict fa _ = choose
    [ Constant <$> fa
    ]
