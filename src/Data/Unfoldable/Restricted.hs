{-# LANGUAGE
    KindSignatures
  , ConstraintKinds
  , MultiParamTypeClasses
  , FunctionalDependencies
  , FlexibleInstances
  #-}

module Data.Unfoldable.Restricted where

import Data.Unfoldable      (Unfoldable (unfold))
import Data.Unfolder        (Unfolder)
import Data.Constraint      (Constraint)
import Data.Constraint.Unit (Unit)





class UnfoldableR (pred :: * -> Constraint) (t :: * -> *) | t -> pred where
  unfoldRestrict :: (pred a, Unfolder f) => f a -> f (t a)

instance ( Unfoldable t
         ) => UnfoldableR Unit t where
  unfoldRestrict = unfold
