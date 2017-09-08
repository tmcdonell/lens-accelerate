{-# LANGUAGE CPP                   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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

module Data.Array.Accelerate.Control.Lens.Lift ( liftLens )
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


-- | Lift a 'Lens' into a lens on the equivalent Accelerate term.
--
-- The instances exported by this package are all defined in terms of this
-- function, using the definitions from the base @lens@ package.
--
liftLens
    :: (Functor f, Unlift box s, Unlift box t, Unlift box b, Lift box a)
    => ((a -> f b) -> s -> f t)
    -> (box (Plain a) -> f (box (Plain b)))
    -> box (Plain s)
    -> f (box (Plain t))
liftLens l f x = lift `fmap` l (fsink1 f) (unlift x)


-- | Sink a unary functor from Accelerate terms
--
fsink1 :: (Functor f, Unlift box b, Lift box a)
       => (box (Plain a) -> f (box (Plain b)))
       -> a
       -> f b
fsink1 f = fmap unlift . f . lift

