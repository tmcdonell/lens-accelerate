{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Accelerate.Control.Lens.Tuple
-- Copyright   : [2015..2020] Trevor L. McDonell
-- License     : BSD-style (see the file LICENSE)
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable
--
----------------------------------------------------------------------------

module Data.Array.Accelerate.Control.Lens.Tuple (

  module Control.Lens.Tuple

) where

import Control.Lens
import Control.Lens.Tuple
import Data.Array.Accelerate
import Data.Array.Accelerate.Control.Lens.Lift


-- Field1
-- ------

instance (Elt a, Elt a', Elt b) => Field1 (Exp (a,b)) (Exp (a',b)) (Exp a) (Exp a') where
  _1 = liftLens (_1 :: Lens (Exp a, Exp b) (Exp a', Exp b) (Exp a) (Exp a'))

instance (Elt a, Elt a', Elt b, Elt c) => Field1 (Exp (a,b,c)) (Exp (a',b,c)) (Exp a) (Exp a') where
  _1 = liftLens (_1 :: Lens (Exp a, Exp b, Exp c) (Exp a', Exp b, Exp c) (Exp a) (Exp a'))

instance (Elt a, Elt a', Elt b, Elt c, Elt d) => Field1 (Exp (a,b,c,d)) (Exp (a',b,c,d)) (Exp a) (Exp a') where
  _1 = liftLens (_1 :: Lens (Exp a, Exp b, Exp c, Exp d) (Exp a', Exp b, Exp c, Exp d) (Exp a) (Exp a'))

instance (Elt a, Elt a', Elt b, Elt c, Elt d, Elt e) => Field1 (Exp (a,b,c,d,e)) (Exp (a',b,c,d,e)) (Exp a) (Exp a') where
  _1 = liftLens (_1 :: Lens (Exp a, Exp b, Exp c, Exp d, Exp e) (Exp a', Exp b, Exp c, Exp d, Exp e) (Exp a) (Exp a'))

instance (Elt a, Elt a', Elt b, Elt c, Elt d, Elt e, Elt f) => Field1 (Exp (a,b,c,d,e,f)) (Exp (a',b,c,d,e,f)) (Exp a) (Exp a') where
  _1 = liftLens (_1 :: Lens (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f) (Exp a', Exp b, Exp c, Exp d, Exp e, Exp f) (Exp a) (Exp a'))

instance (Elt a, Elt a', Elt b, Elt c, Elt d, Elt e, Elt f, Elt g) => Field1 (Exp (a,b,c,d,e,f,g)) (Exp (a',b,c,d,e,f,g)) (Exp a) (Exp a') where
  _1 = liftLens (_1 :: Lens (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g) (Exp a', Exp b, Exp c, Exp d, Exp e, Exp f, Exp g) (Exp a) (Exp a'))

instance (Elt a, Elt a', Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h) => Field1 (Exp (a,b,c,d,e,f,g,h)) (Exp (a',b,c,d,e,f,g,h)) (Exp a) (Exp a') where
  _1 = liftLens (_1 :: Lens (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h) (Exp a', Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h) (Exp a) (Exp a'))

instance (Elt a, Elt a', Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i) => Field1 (Exp (a,b,c,d,e,f,g,h,i)) (Exp (a',b,c,d,e,f,g,h,i)) (Exp a) (Exp a') where
  _1 = liftLens (_1 :: Lens (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h, Exp i) (Exp a', Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h, Exp i) (Exp a) (Exp a'))


instance (Arrays a, Arrays a', Arrays b) => Field1 (Acc (a,b)) (Acc (a',b)) (Acc a) (Acc a') where
  _1 = liftLens (_1 :: Lens (Acc a, Acc b) (Acc a', Acc b) (Acc a) (Acc a'))

instance (Arrays a, Arrays a', Arrays b, Arrays c) => Field1 (Acc (a,b,c)) (Acc (a',b,c)) (Acc a) (Acc a') where
  _1 = liftLens (_1 :: Lens (Acc a, Acc b, Acc c) (Acc a', Acc b, Acc c) (Acc a) (Acc a'))

instance (Arrays a, Arrays a', Arrays b, Arrays c, Arrays d) => Field1 (Acc (a,b,c,d)) (Acc (a',b,c,d)) (Acc a) (Acc a') where
  _1 = liftLens (_1 :: Lens (Acc a, Acc b, Acc c, Acc d) (Acc a', Acc b, Acc c, Acc d) (Acc a) (Acc a'))

instance (Arrays a, Arrays a', Arrays b, Arrays c, Arrays d, Arrays e) => Field1 (Acc (a,b,c,d,e)) (Acc (a',b,c,d,e)) (Acc a) (Acc a') where
  _1 = liftLens (_1 :: Lens (Acc a, Acc b, Acc c, Acc d, Acc e) (Acc a', Acc b, Acc c, Acc d, Acc e) (Acc a) (Acc a'))

instance (Arrays a, Arrays a', Arrays b, Arrays c, Arrays d, Arrays e, Arrays f) => Field1 (Acc (a,b,c,d,e,f)) (Acc (a',b,c,d,e,f)) (Acc a) (Acc a') where
  _1 = liftLens (_1 :: Lens (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f) (Acc a', Acc b, Acc c, Acc d, Acc e, Acc f) (Acc a) (Acc a'))

instance (Arrays a, Arrays a', Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays g) => Field1 (Acc (a,b,c,d,e,f,g)) (Acc (a',b,c,d,e,f,g)) (Acc a) (Acc a') where
  _1 = liftLens (_1 :: Lens (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f, Acc g) (Acc a', Acc b, Acc c, Acc d, Acc e, Acc f, Acc g) (Acc a) (Acc a'))

instance (Arrays a, Arrays a', Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays g, Arrays h) => Field1 (Acc (a,b,c,d,e,f,g,h)) (Acc (a',b,c,d,e,f,g,h)) (Acc a) (Acc a') where
  _1 = liftLens (_1 :: Lens (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f, Acc g, Acc h) (Acc a', Acc b, Acc c, Acc d, Acc e, Acc f, Acc g, Acc h) (Acc a) (Acc a'))

instance (Arrays a, Arrays a', Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays g, Arrays h, Arrays i) => Field1 (Acc (a,b,c,d,e,f,g,h,i)) (Acc (a',b,c,d,e,f,g,h,i)) (Acc a) (Acc a') where
  _1 = liftLens (_1 :: Lens (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f, Acc g, Acc h, Acc i) (Acc a', Acc b, Acc c, Acc d, Acc e, Acc f, Acc g, Acc h, Acc i) (Acc a) (Acc a'))



-- Field2
-- ------

instance (Elt a, Elt b, Elt b') => Field2 (Exp (a,b)) (Exp (a,b')) (Exp b) (Exp b') where
  _2 = liftLens (_2 :: Lens (Exp a, Exp b) (Exp a, Exp b') (Exp b) (Exp b'))

instance (Elt a, Elt b, Elt b', Elt c) => Field2 (Exp (a,b,c)) (Exp (a,b',c)) (Exp b) (Exp b') where
  _2 = liftLens (_2 :: Lens (Exp a, Exp b, Exp c) (Exp a, Exp b', Exp c) (Exp b) (Exp b'))

instance (Elt a, Elt b, Elt b', Elt c, Elt d) => Field2 (Exp (a,b,c,d)) (Exp (a,b',c,d)) (Exp b) (Exp b') where
  _2 = liftLens (_2 :: Lens (Exp a, Exp b, Exp c, Exp d) (Exp a, Exp b', Exp c, Exp d) (Exp b) (Exp b'))

instance (Elt a, Elt b, Elt b', Elt c, Elt d, Elt e) => Field2 (Exp (a,b,c,d,e)) (Exp (a,b',c,d,e)) (Exp b) (Exp b') where
  _2 = liftLens (_2 :: Lens (Exp a, Exp b, Exp c, Exp d, Exp e) (Exp a, Exp b', Exp c, Exp d, Exp e) (Exp b) (Exp b'))

instance (Elt a, Elt b, Elt b', Elt c, Elt d, Elt e, Elt f) => Field2 (Exp (a,b,c,d,e,f)) (Exp (a,b',c,d,e,f)) (Exp b) (Exp b') where
  _2 = liftLens (_2 :: Lens (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f) (Exp a, Exp b', Exp c, Exp d, Exp e, Exp f) (Exp b) (Exp b'))

instance (Elt a, Elt b, Elt b', Elt c, Elt d, Elt e, Elt f, Elt g) => Field2 (Exp (a,b,c,d,e,f,g)) (Exp (a,b',c,d,e,f,g)) (Exp b) (Exp b') where
  _2 = liftLens (_2 :: Lens (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g) (Exp a, Exp b', Exp c, Exp d, Exp e, Exp f, Exp g) (Exp b) (Exp b'))

instance (Elt a, Elt b, Elt b', Elt c, Elt d, Elt e, Elt f, Elt g, Elt h) => Field2 (Exp (a,b,c,d,e,f,g,h)) (Exp (a,b',c,d,e,f,g,h)) (Exp b) (Exp b') where
  _2 = liftLens (_2 :: Lens (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h) (Exp a, Exp b', Exp c, Exp d, Exp e, Exp f, Exp g, Exp h) (Exp b) (Exp b'))

instance (Elt a, Elt b, Elt b', Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i) => Field2 (Exp (a,b,c,d,e,f,g,h,i)) (Exp (a,b',c,d,e,f,g,h,i)) (Exp b) (Exp b') where
  _2 = liftLens (_2 :: Lens (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h, Exp i) (Exp a, Exp b', Exp c, Exp d, Exp e, Exp f, Exp g, Exp h, Exp i) (Exp b) (Exp b'))


instance (Arrays a, Arrays b, Arrays b') => Field2 (Acc (a,b)) (Acc (a,b')) (Acc b) (Acc b') where
  _2 = liftLens (_2 :: Lens (Acc a, Acc b) (Acc a, Acc b') (Acc b) (Acc b'))

instance (Arrays a, Arrays b, Arrays b', Arrays c) => Field2 (Acc (a,b,c)) (Acc (a,b',c)) (Acc b) (Acc b') where
  _2 = liftLens (_2 :: Lens (Acc a, Acc b, Acc c) (Acc a, Acc b', Acc c) (Acc b) (Acc b'))

instance (Arrays a, Arrays b, Arrays b', Arrays c, Arrays d) => Field2 (Acc (a,b,c,d)) (Acc (a,b',c,d)) (Acc b) (Acc b') where
  _2 = liftLens (_2 :: Lens (Acc a, Acc b, Acc c, Acc d) (Acc a, Acc b', Acc c, Acc d) (Acc b) (Acc b'))

instance (Arrays a, Arrays b, Arrays b', Arrays c, Arrays d, Arrays e) => Field2 (Acc (a,b,c,d,e)) (Acc (a,b',c,d,e)) (Acc b) (Acc b') where
  _2 = liftLens (_2 :: Lens (Acc a, Acc b, Acc c, Acc d, Acc e) (Acc a, Acc b', Acc c, Acc d, Acc e) (Acc b) (Acc b'))

instance (Arrays a, Arrays b, Arrays b', Arrays c, Arrays d, Arrays e, Arrays f) => Field2 (Acc (a,b,c,d,e,f)) (Acc (a,b',c,d,e,f)) (Acc b) (Acc b') where
  _2 = liftLens (_2 :: Lens (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f) (Acc a, Acc b', Acc c, Acc d, Acc e, Acc f) (Acc b) (Acc b'))

instance (Arrays a, Arrays b, Arrays b', Arrays c, Arrays d, Arrays e, Arrays f, Arrays g) => Field2 (Acc (a,b,c,d,e,f,g)) (Acc (a,b',c,d,e,f,g)) (Acc b) (Acc b') where
  _2 = liftLens (_2 :: Lens (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f, Acc g) (Acc a, Acc b', Acc c, Acc d, Acc e, Acc f, Acc g) (Acc b) (Acc b'))

instance (Arrays a, Arrays b, Arrays b', Arrays c, Arrays d, Arrays e, Arrays f, Arrays g, Arrays h) => Field2 (Acc (a,b,c,d,e,f,g,h)) (Acc (a,b',c,d,e,f,g,h)) (Acc b) (Acc b') where
  _2 = liftLens (_2 :: Lens (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f, Acc g, Acc h) (Acc a, Acc b', Acc c, Acc d, Acc e, Acc f, Acc g, Acc h) (Acc b) (Acc b'))

instance (Arrays a, Arrays b, Arrays b', Arrays c, Arrays d, Arrays e, Arrays f, Arrays g, Arrays h, Arrays i) => Field2 (Acc (a,b,c,d,e,f,g,h,i)) (Acc (a,b',c,d,e,f,g,h,i)) (Acc b) (Acc b') where
  _2 = liftLens (_2 :: Lens (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f, Acc g, Acc h, Acc i) (Acc a, Acc b', Acc c, Acc d, Acc e, Acc f, Acc g, Acc h, Acc i) (Acc b) (Acc b'))


-- Field3
-- ------

instance (Elt a, Elt b, Elt c, Elt c') => Field3 (Exp (a,b,c)) (Exp (a,b,c')) (Exp c) (Exp c') where
  _3 = liftLens (_3 :: Lens (Exp a, Exp b, Exp c) (Exp a, Exp b, Exp c') (Exp c) (Exp c'))

instance (Elt a, Elt b, Elt c, Elt c', Elt d) => Field3 (Exp (a,b,c,d)) (Exp (a,b,c',d)) (Exp c) (Exp c') where
  _3 = liftLens (_3 :: Lens (Exp a, Exp b, Exp c, Exp d) (Exp a, Exp b, Exp c', Exp d) (Exp c) (Exp c'))

instance (Elt a, Elt b, Elt c, Elt c', Elt d, Elt e) => Field3 (Exp (a,b,c,d,e)) (Exp (a,b,c',d,e)) (Exp c) (Exp c') where
  _3 = liftLens (_3 :: Lens (Exp a, Exp b, Exp c, Exp d, Exp e) (Exp a, Exp b, Exp c', Exp d, Exp e) (Exp c) (Exp c'))

instance (Elt a, Elt b, Elt c, Elt c', Elt d, Elt e, Elt f) => Field3 (Exp (a,b,c,d,e,f)) (Exp (a,b,c',d,e,f)) (Exp c) (Exp c') where
  _3 = liftLens (_3 :: Lens (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f) (Exp a, Exp b, Exp c', Exp d, Exp e, Exp f) (Exp c) (Exp c'))

instance (Elt a, Elt b, Elt c, Elt c', Elt d, Elt e, Elt f, Elt g) => Field3 (Exp (a,b,c,d,e,f,g)) (Exp (a,b,c',d,e,f,g)) (Exp c) (Exp c') where
  _3 = liftLens (_3 :: Lens (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g) (Exp a, Exp b, Exp c', Exp d, Exp e, Exp f, Exp g) (Exp c) (Exp c'))

instance (Elt a, Elt b, Elt c, Elt c', Elt d, Elt e, Elt f, Elt g, Elt h) => Field3 (Exp (a,b,c,d,e,f,g,h)) (Exp (a,b,c',d,e,f,g,h)) (Exp c) (Exp c') where
  _3 = liftLens (_3 :: Lens (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h) (Exp a, Exp b, Exp c', Exp d, Exp e, Exp f, Exp g, Exp h) (Exp c) (Exp c'))

instance (Elt a, Elt b, Elt c, Elt c', Elt d, Elt e, Elt f, Elt g, Elt h, Elt i) => Field3 (Exp (a,b,c,d,e,f,g,h,i)) (Exp (a,b,c',d,e,f,g,h,i)) (Exp c) (Exp c') where
  _3 = liftLens (_3 :: Lens (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h, Exp i) (Exp a, Exp b, Exp c', Exp d, Exp e, Exp f, Exp g, Exp h, Exp i) (Exp c) (Exp c'))


instance (Arrays a, Arrays b, Arrays c, Arrays c') => Field3 (Acc (a,b,c)) (Acc (a,b,c')) (Acc c) (Acc c') where
  _3 = liftLens (_3 :: Lens (Acc a, Acc b, Acc c) (Acc a, Acc b, Acc c') (Acc c) (Acc c'))

instance (Arrays a, Arrays b, Arrays c, Arrays c', Arrays d) => Field3 (Acc (a,b,c,d)) (Acc (a,b,c',d)) (Acc c) (Acc c') where
  _3 = liftLens (_3 :: Lens (Acc a, Acc b, Acc c, Acc d) (Acc a, Acc b, Acc c', Acc d) (Acc c) (Acc c'))

instance (Arrays a, Arrays b, Arrays c, Arrays c', Arrays d, Arrays e) => Field3 (Acc (a,b,c,d,e)) (Acc (a,b,c',d,e)) (Acc c) (Acc c') where
  _3 = liftLens (_3 :: Lens (Acc a, Acc b, Acc c, Acc d, Acc e) (Acc a, Acc b, Acc c', Acc d, Acc e) (Acc c) (Acc c'))

instance (Arrays a, Arrays b, Arrays c, Arrays c', Arrays d, Arrays e, Arrays f) => Field3 (Acc (a,b,c,d,e,f)) (Acc (a,b,c',d,e,f)) (Acc c) (Acc c') where
  _3 = liftLens (_3 :: Lens (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f) (Acc a, Acc b, Acc c', Acc d, Acc e, Acc f) (Acc c) (Acc c'))

instance (Arrays a, Arrays b, Arrays c, Arrays c', Arrays d, Arrays e, Arrays f, Arrays g) => Field3 (Acc (a,b,c,d,e,f,g)) (Acc (a,b,c',d,e,f,g)) (Acc c) (Acc c') where
  _3 = liftLens (_3 :: Lens (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f, Acc g) (Acc a, Acc b, Acc c', Acc d, Acc e, Acc f, Acc g) (Acc c) (Acc c'))

instance (Arrays a, Arrays b, Arrays c, Arrays c', Arrays d, Arrays e, Arrays f, Arrays g, Arrays h) => Field3 (Acc (a,b,c,d,e,f,g,h)) (Acc (a,b,c',d,e,f,g,h)) (Acc c) (Acc c') where
  _3 = liftLens (_3 :: Lens (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f, Acc g, Acc h) (Acc a, Acc b, Acc c', Acc d, Acc e, Acc f, Acc g, Acc h) (Acc c) (Acc c'))

instance (Arrays a, Arrays b, Arrays c, Arrays c', Arrays d, Arrays e, Arrays f, Arrays g, Arrays h, Arrays i) => Field3 (Acc (a,b,c,d,e,f,g,h,i)) (Acc (a,b,c',d,e,f,g,h,i)) (Acc c) (Acc c') where
  _3 = liftLens (_3 :: Lens (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f, Acc g, Acc h, Acc i) (Acc a, Acc b, Acc c', Acc d, Acc e, Acc f, Acc g, Acc h, Acc i) (Acc c) (Acc c'))


-- Field4
-- ------

instance (Elt a, Elt b, Elt c, Elt d, Elt d') => Field4 (Exp (a,b,c,d)) (Exp (a,b,c,d')) (Exp d) (Exp d') where
  _4 = liftLens (_4 :: Lens (Exp a, Exp b, Exp c, Exp d) (Exp a, Exp b, Exp c, Exp d') (Exp d) (Exp d'))

instance (Elt a, Elt b, Elt c, Elt d, Elt d', Elt e) => Field4 (Exp (a,b,c,d,e)) (Exp (a,b,c,d',e)) (Exp d) (Exp d') where
  _4 = liftLens (_4 :: Lens (Exp a, Exp b, Exp c, Exp d, Exp e) (Exp a, Exp b, Exp c, Exp d', Exp e) (Exp d) (Exp d'))

instance (Elt a, Elt b, Elt c, Elt d, Elt d', Elt e, Elt f) => Field4 (Exp (a,b,c,d,e,f)) (Exp (a,b,c,d',e,f)) (Exp d) (Exp d') where
  _4 = liftLens (_4 :: Lens (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f) (Exp a, Exp b, Exp c, Exp d', Exp e, Exp f) (Exp d) (Exp d'))

instance (Elt a, Elt b, Elt c, Elt d, Elt d', Elt e, Elt f, Elt g) => Field4 (Exp (a,b,c,d,e,f,g)) (Exp (a,b,c,d',e,f,g)) (Exp d) (Exp d') where
  _4 = liftLens (_4 :: Lens (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g) (Exp a, Exp b, Exp c, Exp d', Exp e, Exp f, Exp g) (Exp d) (Exp d'))

instance (Elt a, Elt b, Elt c, Elt d, Elt d', Elt e, Elt f, Elt g, Elt h) => Field4 (Exp (a,b,c,d,e,f,g,h)) (Exp (a,b,c,d',e,f,g,h)) (Exp d) (Exp d') where
  _4 = liftLens (_4 :: Lens (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h) (Exp a, Exp b, Exp c, Exp d', Exp e, Exp f, Exp g, Exp h) (Exp d) (Exp d'))

instance (Elt a, Elt b, Elt c, Elt d, Elt d', Elt e, Elt f, Elt g, Elt h, Elt i) => Field4 (Exp (a,b,c,d,e,f,g,h,i)) (Exp (a,b,c,d',e,f,g,h,i)) (Exp d) (Exp d') where
  _4 = liftLens (_4 :: Lens (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h, Exp i) (Exp a, Exp b, Exp c, Exp d', Exp e, Exp f, Exp g, Exp h, Exp i) (Exp d) (Exp d'))


instance (Arrays a, Arrays b, Arrays c, Arrays d, Arrays d') => Field4 (Acc (a,b,c,d)) (Acc (a,b,c,d')) (Acc d) (Acc d') where
  _4 = liftLens (_4 :: Lens (Acc a, Acc b, Acc c, Acc d) (Acc a, Acc b, Acc c, Acc d') (Acc d) (Acc d'))

instance (Arrays a, Arrays b, Arrays c, Arrays d, Arrays d', Arrays e) => Field4 (Acc (a,b,c,d,e)) (Acc (a,b,c,d',e)) (Acc d) (Acc d') where
  _4 = liftLens (_4 :: Lens (Acc a, Acc b, Acc c, Acc d, Acc e) (Acc a, Acc b, Acc c, Acc d', Acc e) (Acc d) (Acc d'))

instance (Arrays a, Arrays b, Arrays c, Arrays d, Arrays d', Arrays e, Arrays f) => Field4 (Acc (a,b,c,d,e,f)) (Acc (a,b,c,d',e,f)) (Acc d) (Acc d') where
  _4 = liftLens (_4 :: Lens (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f) (Acc a, Acc b, Acc c, Acc d', Acc e, Acc f) (Acc d) (Acc d'))

instance (Arrays a, Arrays b, Arrays c, Arrays d, Arrays d', Arrays e, Arrays f, Arrays g) => Field4 (Acc (a,b,c,d,e,f,g)) (Acc (a,b,c,d',e,f,g)) (Acc d) (Acc d') where
  _4 = liftLens (_4 :: Lens (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f, Acc g) (Acc a, Acc b, Acc c, Acc d', Acc e, Acc f, Acc g) (Acc d) (Acc d'))

instance (Arrays a, Arrays b, Arrays c, Arrays d, Arrays d', Arrays e, Arrays f, Arrays g, Arrays h) => Field4 (Acc (a,b,c,d,e,f,g,h)) (Acc (a,b,c,d',e,f,g,h)) (Acc d) (Acc d') where
  _4 = liftLens (_4 :: Lens (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f, Acc g, Acc h) (Acc a, Acc b, Acc c, Acc d', Acc e, Acc f, Acc g, Acc h) (Acc d) (Acc d'))

instance (Arrays a, Arrays b, Arrays c, Arrays d, Arrays d', Arrays e, Arrays f, Arrays g, Arrays h, Arrays i) => Field4 (Acc (a,b,c,d,e,f,g,h,i)) (Acc (a,b,c,d',e,f,g,h,i)) (Acc d) (Acc d') where
  _4 = liftLens (_4 :: Lens (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f, Acc g, Acc h, Acc i) (Acc a, Acc b, Acc c, Acc d', Acc e, Acc f, Acc g, Acc h, Acc i) (Acc d) (Acc d'))


-- Field5
-- ------

instance (Elt a, Elt b, Elt c, Elt d, Elt e, Elt e') => Field5 (Exp (a,b,c,d,e)) (Exp (a,b,c,d,e')) (Exp e) (Exp e') where
  _5 = liftLens (_5 :: Lens (Exp a, Exp b, Exp c, Exp d, Exp e) (Exp a, Exp b, Exp c, Exp d, Exp e') (Exp e) (Exp e'))

instance (Elt a, Elt b, Elt c, Elt d, Elt e, Elt e', Elt f) => Field5 (Exp (a,b,c,d,e,f)) (Exp (a,b,c,d,e',f)) (Exp e) (Exp e') where
  _5 = liftLens (_5 :: Lens (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f) (Exp a, Exp b, Exp c, Exp d, Exp e', Exp f) (Exp e) (Exp e'))

instance (Elt a, Elt b, Elt c, Elt d, Elt e, Elt e', Elt f, Elt g) => Field5 (Exp (a,b,c,d,e,f,g)) (Exp (a,b,c,d,e',f,g)) (Exp e) (Exp e') where
  _5 = liftLens (_5 :: Lens (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g) (Exp a, Exp b, Exp c, Exp d, Exp e', Exp f, Exp g) (Exp e) (Exp e'))

instance (Elt a, Elt b, Elt c, Elt d, Elt e, Elt e', Elt f, Elt g, Elt h) => Field5 (Exp (a,b,c,d,e,f,g,h)) (Exp (a,b,c,d,e',f,g,h)) (Exp e) (Exp e') where
  _5 = liftLens (_5 :: Lens (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h) (Exp a, Exp b, Exp c, Exp d, Exp e', Exp f, Exp g, Exp h) (Exp e) (Exp e'))

instance (Elt a, Elt b, Elt c, Elt d, Elt e, Elt e', Elt f, Elt g, Elt h, Elt i) => Field5 (Exp (a,b,c,d,e,f,g,h,i)) (Exp (a,b,c,d,e',f,g,h,i)) (Exp e) (Exp e') where
  _5 = liftLens (_5 :: Lens (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h, Exp i) (Exp a, Exp b, Exp c, Exp d, Exp e', Exp f, Exp g, Exp h, Exp i) (Exp e) (Exp e'))


instance (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays e') => Field5 (Acc (a,b,c,d,e)) (Acc (a,b,c,d,e')) (Acc e) (Acc e') where
  _5 = liftLens (_5 :: Lens (Acc a, Acc b, Acc c, Acc d, Acc e) (Acc a, Acc b, Acc c, Acc d, Acc e') (Acc e) (Acc e'))

instance (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays e', Arrays f) => Field5 (Acc (a,b,c,d,e,f)) (Acc (a,b,c,d,e',f)) (Acc e) (Acc e') where
  _5 = liftLens (_5 :: Lens (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f) (Acc a, Acc b, Acc c, Acc d, Acc e', Acc f) (Acc e) (Acc e'))

instance (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays e', Arrays f, Arrays g) => Field5 (Acc (a,b,c,d,e,f,g)) (Acc (a,b,c,d,e',f,g)) (Acc e) (Acc e') where
  _5 = liftLens (_5 :: Lens (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f, Acc g) (Acc a, Acc b, Acc c, Acc d, Acc e', Acc f, Acc g) (Acc e) (Acc e'))

instance (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays e', Arrays f, Arrays g, Arrays h) => Field5 (Acc (a,b,c,d,e,f,g,h)) (Acc (a,b,c,d,e',f,g,h)) (Acc e) (Acc e') where
  _5 = liftLens (_5 :: Lens (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f, Acc g, Acc h) (Acc a, Acc b, Acc c, Acc d, Acc e', Acc f, Acc g, Acc h) (Acc e) (Acc e'))

instance (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays e', Arrays f, Arrays g, Arrays h, Arrays i) => Field5 (Acc (a,b,c,d,e,f,g,h,i)) (Acc (a,b,c,d,e',f,g,h,i)) (Acc e) (Acc e') where
  _5 = liftLens (_5 :: Lens (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f, Acc g, Acc h, Acc i) (Acc a, Acc b, Acc c, Acc d, Acc e', Acc f, Acc g, Acc h, Acc i) (Acc e) (Acc e'))


-- Field6
-- ------

instance (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt f') => Field6 (Exp (a,b,c,d,e,f)) (Exp (a,b,c,d,e,f')) (Exp f) (Exp f') where
  _6 = liftLens (_6 :: Lens (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f) (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f') (Exp f) (Exp f'))

instance (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt f', Elt g) => Field6 (Exp (a,b,c,d,e,f,g)) (Exp (a,b,c,d,e,f',g)) (Exp f) (Exp f') where
  _6 = liftLens (_6 :: Lens (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g) (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f', Exp g) (Exp f) (Exp f'))

instance (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt f', Elt g, Elt h) => Field6 (Exp (a,b,c,d,e,f,g,h)) (Exp (a,b,c,d,e,f',g,h)) (Exp f) (Exp f') where
  _6 = liftLens (_6 :: Lens (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h) (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f', Exp g, Exp h) (Exp f) (Exp f'))

instance (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt f', Elt g, Elt h, Elt i) => Field6 (Exp (a,b,c,d,e,f,g,h,i)) (Exp (a,b,c,d,e,f',g,h,i)) (Exp f) (Exp f') where
  _6 = liftLens (_6 :: Lens (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h, Exp i) (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f', Exp g, Exp h, Exp i) (Exp f) (Exp f'))


instance (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays f') => Field6 (Acc (a,b,c,d,e,f)) (Acc (a,b,c,d,e,f')) (Acc f) (Acc f') where
  _6 = liftLens (_6 :: Lens (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f) (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f') (Acc f) (Acc f'))

instance (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays f', Arrays g) => Field6 (Acc (a,b,c,d,e,f,g)) (Acc (a,b,c,d,e,f',g)) (Acc f) (Acc f') where
  _6 = liftLens (_6 :: Lens (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f, Acc g) (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f', Acc g) (Acc f) (Acc f'))

instance (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays f', Arrays g, Arrays h) => Field6 (Acc (a,b,c,d,e,f,g,h)) (Acc (a,b,c,d,e,f',g,h)) (Acc f) (Acc f') where
  _6 = liftLens (_6 :: Lens (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f, Acc g, Acc h) (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f', Acc g, Acc h) (Acc f) (Acc f'))

instance (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays f', Arrays g, Arrays h, Arrays i) => Field6 (Acc (a,b,c,d,e,f,g,h,i)) (Acc (a,b,c,d,e,f',g,h,i)) (Acc f) (Acc f') where
  _6 = liftLens (_6 :: Lens (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f, Acc g, Acc h, Acc i) (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f', Acc g, Acc h, Acc i) (Acc f) (Acc f'))


-- Field7
-- ------

instance (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt g') => Field7 (Exp (a,b,c,d,e,f,g)) (Exp (a,b,c,d,e,f,g')) (Exp g) (Exp g') where
  _7 = liftLens (_7 :: Lens (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g) (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g') (Exp g) (Exp g'))

instance (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt g', Elt h) => Field7 (Exp (a,b,c,d,e,f,g,h)) (Exp (a,b,c,d,e,f,g',h)) (Exp g) (Exp g') where
  _7 = liftLens (_7 :: Lens (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h) (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g', Exp h) (Exp g) (Exp g'))

instance (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt g', Elt h, Elt i) => Field7 (Exp (a,b,c,d,e,f,g,h,i)) (Exp (a,b,c,d,e,f,g',h,i)) (Exp g) (Exp g') where
  _7 = liftLens (_7 :: Lens (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h, Exp i) (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g', Exp h, Exp i) (Exp g) (Exp g'))


instance (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays g, Arrays g') => Field7 (Acc (a,b,c,d,e,f,g)) (Acc (a,b,c,d,e,f,g')) (Acc g) (Acc g') where
  _7 = liftLens (_7 :: Lens (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f, Acc g) (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f, Acc g') (Acc g) (Acc g'))

instance (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays g, Arrays g', Arrays h) => Field7 (Acc (a,b,c,d,e,f,g,h)) (Acc (a,b,c,d,e,f,g',h)) (Acc g) (Acc g') where
  _7 = liftLens (_7 :: Lens (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f, Acc g, Acc h) (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f, Acc g', Acc h) (Acc g) (Acc g'))

instance (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays g, Arrays g', Arrays h, Arrays i) => Field7 (Acc (a,b,c,d,e,f,g,h,i)) (Acc (a,b,c,d,e,f,g',h,i)) (Acc g) (Acc g') where
  _7 = liftLens (_7 :: Lens (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f, Acc g, Acc h, Acc i) (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f, Acc g', Acc h, Acc i) (Acc g) (Acc g'))


-- Field8
-- ------

instance (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt h') => Field8 (Exp (a,b,c,d,e,f,g,h)) (Exp (a,b,c,d,e,f,g,h')) (Exp h) (Exp h') where
  _8 = liftLens (_8 :: Lens (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h) (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h') (Exp h) (Exp h'))

instance (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt h', Elt i) => Field8 (Exp (a,b,c,d,e,f,g,h,i)) (Exp (a,b,c,d,e,f,g,h',i)) (Exp h) (Exp h') where
  _8 = liftLens (_8 :: Lens (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h, Exp i) (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h', Exp i) (Exp h) (Exp h'))


instance (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays g, Arrays h, Arrays h') => Field8 (Acc (a,b,c,d,e,f,g,h)) (Acc (a,b,c,d,e,f,g,h')) (Acc h) (Acc h') where
  _8 = liftLens (_8 :: Lens (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f, Acc g, Acc h) (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f, Acc g, Acc h') (Acc h) (Acc h'))

instance (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays g, Arrays h, Arrays h', Arrays i) => Field8 (Acc (a,b,c,d,e,f,g,h,i)) (Acc (a,b,c,d,e,f,g,h',i)) (Acc h) (Acc h') where
  _8 = liftLens (_8 :: Lens (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f, Acc g, Acc h, Acc i) (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f, Acc g, Acc h', Acc i) (Acc h) (Acc h'))


-- Field9
-- ------

instance (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i, Elt i') => Field9 (Exp (a,b,c,d,e,f,g,h,i)) (Exp (a,b,c,d,e,f,g,h,i')) (Exp i) (Exp i') where
  _9 = liftLens (_9 :: Lens (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h, Exp i) (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h, Exp i') (Exp i) (Exp i'))


instance (Arrays a, Arrays b, Arrays c, Arrays d, Arrays e, Arrays f, Arrays g, Arrays h, Arrays i, Arrays i') => Field9 (Acc (a,b,c,d,e,f,g,h,i)) (Acc (a,b,c,d,e,f,g,h,i')) (Acc i) (Acc i') where
  _9 = liftLens (_9 :: Lens (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f, Acc g, Acc h, Acc i) (Acc a, Acc b, Acc c, Acc d, Acc e, Acc f, Acc g, Acc h, Acc i') (Acc i) (Acc i'))

