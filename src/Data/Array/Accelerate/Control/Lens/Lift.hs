{-# LANGUAGE CPP          #-}
{-# LANGUAGE ViewPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Accelerate.Control.Lens.Lift
-- Copyright   : 2015 Trevor L. McDonell
-- License     : BSD-style (see the file LICENSE)
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable
--
----------------------------------------------------------------------------

module Data.Array.Accelerate.Control.Lens.Lift
  where

import Data.Array.Accelerate


#if defined(MIN_VERSION_accelerate)
#if !MIN_VERSION_accelerate(0,16,0)
-- Instances that missed the 0.15* Hackage release
instance Unlift Exp (Exp e) where
  unlift = id

instance Unlift Acc (Acc a) where
  unlift = id
#endif
#endif

-- | Lift a 'Lens' into Accelerate terms
--
liftLens
    :: (Functor f, Unlift box s, Unlift box t)
    => (l -> s -> f t)
    -> l
    -> box (Plain s)
    -> f (box (Plain t))
liftLens l f (unlift -> x) = lift `fmap` l f x


-- | Sink a unary functor from Accelerate terms
--
fsink1 :: (Functor f, Unlift box b, Lift box a)
       => (box (Plain a) -> f (box (Plain b)))
       -> a
       -> f b
fsink1 f = fmap unlift . f . lift

