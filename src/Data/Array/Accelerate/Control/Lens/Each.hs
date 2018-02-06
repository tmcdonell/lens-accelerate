{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Accelerate.Control.Lens.Each
-- Copyright   : 2015 Trevor L. McDonell
-- License     : BSD-style (see the file LICENSE)
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable
--
----------------------------------------------------------------------------

module Data.Array.Accelerate.Control.Lens.Each (

  module Control.Lens.Each

) where

import Control.Lens.Each
import Control.Lens.Traversal
import Data.Array.Accelerate
import Data.Array.Accelerate.Data.Complex
import Data.Array.Accelerate.Control.Lens.Lift


instance (Elt a, Elt b, Elt (Complex a), Elt (Complex b)) => Each (Exp (Complex a)) (Exp (Complex b)) (Exp a) (Exp b) where
  each = liftLens (each :: Traversal (Complex (Exp a)) (Complex (Exp b)) (Exp a) (Exp b))

instance (Elt a, Elt b) => Each (Exp (a,a)) (Exp (b,b)) (Exp a) (Exp b) where
  each = liftLens (each :: Traversal (Exp a, Exp a) (Exp b, Exp b) (Exp a) (Exp b))

instance (Elt a, Elt b) => Each (Exp (a,a,a)) (Exp (b,b,b)) (Exp a) (Exp b) where
  each = liftLens (each :: Traversal (Exp a, Exp a, Exp a) (Exp b, Exp b, Exp b) (Exp a) (Exp b))

instance (Elt a, Elt b) => Each (Exp (a,a,a,a)) (Exp (b,b,b,b)) (Exp a) (Exp b) where
  each = liftLens (each :: Traversal (Exp a, Exp a, Exp a, Exp a) (Exp b, Exp b, Exp b, Exp b) (Exp a) (Exp b))

instance (Elt a, Elt b) => Each (Exp (a,a,a,a,a)) (Exp (b,b,b,b,b)) (Exp a) (Exp b) where
  each = liftLens (each :: Traversal (Exp a, Exp a, Exp a, Exp a, Exp a) (Exp b, Exp b, Exp b, Exp b, Exp b) (Exp a) (Exp b))

instance (Elt a, Elt b) => Each (Exp (a,a,a,a,a,a)) (Exp (b,b,b,b,b,b)) (Exp a) (Exp b) where
  each = liftLens (each :: Traversal (Exp a, Exp a, Exp a, Exp a, Exp a, Exp a) (Exp b, Exp b, Exp b, Exp b, Exp b, Exp b) (Exp a) (Exp b))

instance (Elt a, Elt b) => Each (Exp (a,a,a,a,a,a,a)) (Exp (b,b,b,b,b,b,b)) (Exp a) (Exp b) where
  each = liftLens (each :: Traversal (Exp a, Exp a, Exp a, Exp a, Exp a, Exp a, Exp a) (Exp b, Exp b, Exp b, Exp b, Exp b, Exp b, Exp b) (Exp a) (Exp b))

instance (Elt a, Elt b) => Each (Exp (a,a,a,a,a,a,a,a)) (Exp (b,b,b,b,b,b,b,b)) (Exp a) (Exp b) where
  each = liftLens (each :: Traversal (Exp a, Exp a, Exp a, Exp a, Exp a, Exp a, Exp a, Exp a) (Exp b, Exp b, Exp b, Exp b, Exp b, Exp b, Exp b, Exp b) (Exp a) (Exp b))

instance (Elt a, Elt b) => Each (Exp (a,a,a,a,a,a,a,a,a)) (Exp (b,b,b,b,b,b,b,b,b)) (Exp a) (Exp b) where
  each = liftLens (each :: Traversal (Exp a, Exp a, Exp a, Exp a, Exp a, Exp a, Exp a, Exp a, Exp a) (Exp b, Exp b, Exp b, Exp b, Exp b, Exp b, Exp b, Exp b, Exp b) (Exp a) (Exp b))


instance (Arrays a, Arrays b) => Each (Acc (a,a)) (Acc (b,b)) (Acc a) (Acc b) where
  each = liftLens (each :: Traversal (Acc a, Acc a) (Acc b, Acc b) (Acc a) (Acc b))

instance (Arrays a, Arrays b) => Each (Acc (a,a,a)) (Acc (b,b,b)) (Acc a) (Acc b) where
  each = liftLens (each :: Traversal (Acc a, Acc a, Acc a) (Acc b, Acc b, Acc b) (Acc a) (Acc b))

instance (Arrays a, Arrays b) => Each (Acc (a,a,a,a)) (Acc (b,b,b,b)) (Acc a) (Acc b) where
  each = liftLens (each :: Traversal (Acc a, Acc a, Acc a, Acc a) (Acc b, Acc b, Acc b, Acc b) (Acc a) (Acc b))

instance (Arrays a, Arrays b) => Each (Acc (a,a,a,a,a)) (Acc (b,b,b,b,b)) (Acc a) (Acc b) where
  each = liftLens (each :: Traversal (Acc a, Acc a, Acc a, Acc a, Acc a) (Acc b, Acc b, Acc b, Acc b, Acc b) (Acc a) (Acc b))

instance (Arrays a, Arrays b) => Each (Acc (a,a,a,a,a,a)) (Acc (b,b,b,b,b,b)) (Acc a) (Acc b) where
  each = liftLens (each :: Traversal (Acc a, Acc a, Acc a, Acc a, Acc a, Acc a) (Acc b, Acc b, Acc b, Acc b, Acc b, Acc b) (Acc a) (Acc b))

instance (Arrays a, Arrays b) => Each (Acc (a,a,a,a,a,a,a)) (Acc (b,b,b,b,b,b,b)) (Acc a) (Acc b) where
  each = liftLens (each :: Traversal (Acc a, Acc a, Acc a, Acc a, Acc a, Acc a, Acc a) (Acc b, Acc b, Acc b, Acc b, Acc b, Acc b, Acc b) (Acc a) (Acc b))

instance (Arrays a, Arrays b) => Each (Acc (a,a,a,a,a,a,a,a)) (Acc (b,b,b,b,b,b,b,b)) (Acc a) (Acc b) where
  each = liftLens (each :: Traversal (Acc a, Acc a, Acc a, Acc a, Acc a, Acc a, Acc a, Acc a) (Acc b, Acc b, Acc b, Acc b, Acc b, Acc b, Acc b, Acc b) (Acc a) (Acc b))

instance (Arrays a, Arrays b) => Each (Acc (a,a,a,a,a,a,a,a,a)) (Acc (b,b,b,b,b,b,b,b,b)) (Acc a) (Acc b) where
  each = liftLens (each :: Traversal (Acc a, Acc a, Acc a, Acc a, Acc a, Acc a, Acc a, Acc a, Acc a) (Acc b, Acc b, Acc b, Acc b, Acc b, Acc b, Acc b, Acc b, Acc b) (Acc a) (Acc b))

