{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Accelerate.Control.Lens.Tuple
-- Copyright   : 2015 Trevor L. McDonell
-- License     : BSD-style (see the file LICENSE)
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable
--
----------------------------------------------------------------------------

module Data.Array.Accelerate.Control.Lens.Tuple ()
  where

import Control.Lens
import Control.Lens.Tuple                               as L
import Data.Array.Accelerate                            as A


-- Field1
-- ------

instance (Elt a, Elt a', Elt b) => Field1 (Exp (a,b)) (Exp (a',b)) (Exp a) (Exp a') where
  _1 f = liftLens (L._1 :: Lens (Exp a, Exp b) (Exp a', Exp b) (Exp a) (Exp a')) (fsink1 f)

instance (Elt a, Elt a', Elt b, Elt c) => Field1 (Exp (a,b,c)) (Exp (a',b,c)) (Exp a) (Exp a') where
  _1 f = liftLens (L._1 :: Lens (Exp a, Exp b, Exp c) (Exp a', Exp b, Exp c) (Exp a) (Exp a')) (fsink1 f)

instance (Elt a, Elt a', Elt b, Elt c, Elt d) => Field1 (Exp (a,b,c,d)) (Exp (a',b,c,d)) (Exp a) (Exp a') where
  _1 f = liftLens (L._1 :: Lens (Exp a, Exp b, Exp c, Exp d) (Exp a', Exp b, Exp c, Exp d) (Exp a) (Exp a')) (fsink1 f)

instance (Elt a, Elt a', Elt b, Elt c, Elt d, Elt e) => Field1 (Exp (a,b,c,d,e)) (Exp (a',b,c,d,e)) (Exp a) (Exp a') where
  _1 f = liftLens (L._1 :: Lens (Exp a, Exp b, Exp c, Exp d, Exp e) (Exp a', Exp b, Exp c, Exp d, Exp e) (Exp a) (Exp a')) (fsink1 f)

instance (Elt a, Elt a', Elt b, Elt c, Elt d, Elt e, Elt f) => Field1 (Exp (a,b,c,d,e,f)) (Exp (a',b,c,d,e,f)) (Exp a) (Exp a') where
  _1 f = liftLens (L._1 :: Lens (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f) (Exp a', Exp b, Exp c, Exp d, Exp e, Exp f) (Exp a) (Exp a')) (fsink1 f)

instance (Elt a, Elt a', Elt b, Elt c, Elt d, Elt e, Elt f, Elt g) => Field1 (Exp (a,b,c,d,e,f,g)) (Exp (a',b,c,d,e,f,g)) (Exp a) (Exp a') where
  _1 f = liftLens (L._1 :: Lens (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g) (Exp a', Exp b, Exp c, Exp d, Exp e, Exp f, Exp g) (Exp a) (Exp a')) (fsink1 f)

instance (Elt a, Elt a', Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h) => Field1 (Exp (a,b,c,d,e,f,g,h)) (Exp (a',b,c,d,e,f,g,h)) (Exp a) (Exp a') where
  _1 f = liftLens (L._1 :: Lens (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h) (Exp a', Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h) (Exp a) (Exp a')) (fsink1 f)

instance (Elt a, Elt a', Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i) => Field1 (Exp (a,b,c,d,e,f,g,h,i)) (Exp (a',b,c,d,e,f,g,h,i)) (Exp a) (Exp a') where
  _1 f = liftLens (L._1 :: Lens (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h, Exp i) (Exp a', Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h, Exp i) (Exp a) (Exp a')) (fsink1 f)



-- Field2
-- ------

instance (Elt a, Elt b, Elt b') => Field2 (Exp (a,b)) (Exp (a,b')) (Exp b) (Exp b') where
  _2 f = liftLens (L._2 :: Lens (Exp a, Exp b) (Exp a, Exp b') (Exp b) (Exp b')) (fsink1 f)

instance (Elt a, Elt b, Elt b', Elt c) => Field2 (Exp (a,b,c)) (Exp (a,b',c)) (Exp b) (Exp b') where
  _2 f = liftLens (L._2 :: Lens (Exp a, Exp b, Exp c) (Exp a, Exp b', Exp c) (Exp b) (Exp b')) (fsink1 f)

instance (Elt a, Elt b, Elt b', Elt c, Elt d) => Field2 (Exp (a,b,c,d)) (Exp (a,b',c,d)) (Exp b) (Exp b') where
  _2 f = liftLens (L._2 :: Lens (Exp a, Exp b, Exp c, Exp d) (Exp a, Exp b', Exp c, Exp d) (Exp b) (Exp b')) (fsink1 f)

instance (Elt a, Elt b, Elt b', Elt c, Elt d, Elt e) => Field2 (Exp (a,b,c,d,e)) (Exp (a,b',c,d,e)) (Exp b) (Exp b') where
  _2 f = liftLens (L._2 :: Lens (Exp a, Exp b, Exp c, Exp d, Exp e) (Exp a, Exp b', Exp c, Exp d, Exp e) (Exp b) (Exp b')) (fsink1 f)

instance (Elt a, Elt b, Elt b', Elt c, Elt d, Elt e, Elt f) => Field2 (Exp (a,b,c,d,e,f)) (Exp (a,b',c,d,e,f)) (Exp b) (Exp b') where
  _2 f = liftLens (L._2 :: Lens (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f) (Exp a, Exp b', Exp c, Exp d, Exp e, Exp f) (Exp b) (Exp b')) (fsink1 f)

instance (Elt a, Elt b, Elt b', Elt c, Elt d, Elt e, Elt f, Elt g) => Field2 (Exp (a,b,c,d,e,f,g)) (Exp (a,b',c,d,e,f,g)) (Exp b) (Exp b') where
  _2 f = liftLens (L._2 :: Lens (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g) (Exp a, Exp b', Exp c, Exp d, Exp e, Exp f, Exp g) (Exp b) (Exp b')) (fsink1 f)

instance (Elt a, Elt b, Elt b', Elt c, Elt d, Elt e, Elt f, Elt g, Elt h) => Field2 (Exp (a,b,c,d,e,f,g,h)) (Exp (a,b',c,d,e,f,g,h)) (Exp b) (Exp b') where
  _2 f = liftLens (L._2 :: Lens (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h) (Exp a, Exp b', Exp c, Exp d, Exp e, Exp f, Exp g, Exp h) (Exp b) (Exp b')) (fsink1 f)

instance (Elt a, Elt b, Elt b', Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i) => Field2 (Exp (a,b,c,d,e,f,g,h,i)) (Exp (a,b',c,d,e,f,g,h,i)) (Exp b) (Exp b') where
  _2 f = liftLens (L._2 :: Lens (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h, Exp i) (Exp a, Exp b', Exp c, Exp d, Exp e, Exp f, Exp g, Exp h, Exp i) (Exp b) (Exp b')) (fsink1 f)


-- Field3
-- ------

instance (Elt a, Elt b, Elt c, Elt c') => Field3 (Exp (a,b,c)) (Exp (a,b,c')) (Exp c) (Exp c') where
  _3 f = liftLens (L._3 :: Lens (Exp a, Exp b, Exp c) (Exp a, Exp b, Exp c') (Exp c) (Exp c')) (fsink1 f)

instance (Elt a, Elt b, Elt c, Elt c', Elt d) => Field3 (Exp (a,b,c,d)) (Exp (a,b,c',d)) (Exp c) (Exp c') where
  _3 f = liftLens (L._3 :: Lens (Exp a, Exp b, Exp c, Exp d) (Exp a, Exp b, Exp c', Exp d) (Exp c) (Exp c')) (fsink1 f)

instance (Elt a, Elt b, Elt c, Elt c', Elt d, Elt e) => Field3 (Exp (a,b,c,d,e)) (Exp (a,b,c',d,e)) (Exp c) (Exp c') where
  _3 f = liftLens (L._3 :: Lens (Exp a, Exp b, Exp c, Exp d, Exp e) (Exp a, Exp b, Exp c', Exp d, Exp e) (Exp c) (Exp c')) (fsink1 f)

instance (Elt a, Elt b, Elt c, Elt c', Elt d, Elt e, Elt f) => Field3 (Exp (a,b,c,d,e,f)) (Exp (a,b,c',d,e,f)) (Exp c) (Exp c') where
  _3 f = liftLens (L._3 :: Lens (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f) (Exp a, Exp b, Exp c', Exp d, Exp e, Exp f) (Exp c) (Exp c')) (fsink1 f)

instance (Elt a, Elt b, Elt c, Elt c', Elt d, Elt e, Elt f, Elt g) => Field3 (Exp (a,b,c,d,e,f,g)) (Exp (a,b,c',d,e,f,g)) (Exp c) (Exp c') where
  _3 f = liftLens (L._3 :: Lens (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g) (Exp a, Exp b, Exp c', Exp d, Exp e, Exp f, Exp g) (Exp c) (Exp c')) (fsink1 f)

instance (Elt a, Elt b, Elt c, Elt c', Elt d, Elt e, Elt f, Elt g, Elt h) => Field3 (Exp (a,b,c,d,e,f,g,h)) (Exp (a,b,c',d,e,f,g,h)) (Exp c) (Exp c') where
  _3 f = liftLens (L._3 :: Lens (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h) (Exp a, Exp b, Exp c', Exp d, Exp e, Exp f, Exp g, Exp h) (Exp c) (Exp c')) (fsink1 f)

instance (Elt a, Elt b, Elt c, Elt c', Elt d, Elt e, Elt f, Elt g, Elt h, Elt i) => Field3 (Exp (a,b,c,d,e,f,g,h,i)) (Exp (a,b,c',d,e,f,g,h,i)) (Exp c) (Exp c') where
  _3 f = liftLens (L._3 :: Lens (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h, Exp i) (Exp a, Exp b, Exp c', Exp d, Exp e, Exp f, Exp g, Exp h, Exp i) (Exp c) (Exp c')) (fsink1 f)


-- Field4
-- ------

instance (Elt a, Elt b, Elt c, Elt d, Elt d') => Field4 (Exp (a,b,c,d)) (Exp (a,b,c,d')) (Exp d) (Exp d') where
  _4 f = liftLens (L._4 :: Lens (Exp a, Exp b, Exp c, Exp d) (Exp a, Exp b, Exp c, Exp d') (Exp d) (Exp d')) (fsink1 f)

instance (Elt a, Elt b, Elt c, Elt d, Elt d', Elt e) => Field4 (Exp (a,b,c,d,e)) (Exp (a,b,c,d',e)) (Exp d) (Exp d') where
  _4 f = liftLens (L._4 :: Lens (Exp a, Exp b, Exp c, Exp d, Exp e) (Exp a, Exp b, Exp c, Exp d', Exp e) (Exp d) (Exp d')) (fsink1 f)

instance (Elt a, Elt b, Elt c, Elt d, Elt d', Elt e, Elt f) => Field4 (Exp (a,b,c,d,e,f)) (Exp (a,b,c,d',e,f)) (Exp d) (Exp d') where
  _4 f = liftLens (L._4 :: Lens (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f) (Exp a, Exp b, Exp c, Exp d', Exp e, Exp f) (Exp d) (Exp d')) (fsink1 f)

instance (Elt a, Elt b, Elt c, Elt d, Elt d', Elt e, Elt f, Elt g) => Field4 (Exp (a,b,c,d,e,f,g)) (Exp (a,b,c,d',e,f,g)) (Exp d) (Exp d') where
  _4 f = liftLens (L._4 :: Lens (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g) (Exp a, Exp b, Exp c, Exp d', Exp e, Exp f, Exp g) (Exp d) (Exp d')) (fsink1 f)

instance (Elt a, Elt b, Elt c, Elt d, Elt d', Elt e, Elt f, Elt g, Elt h) => Field4 (Exp (a,b,c,d,e,f,g,h)) (Exp (a,b,c,d',e,f,g,h)) (Exp d) (Exp d') where
  _4 f = liftLens (L._4 :: Lens (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h) (Exp a, Exp b, Exp c, Exp d', Exp e, Exp f, Exp g, Exp h) (Exp d) (Exp d')) (fsink1 f)

instance (Elt a, Elt b, Elt c, Elt d, Elt d', Elt e, Elt f, Elt g, Elt h, Elt i) => Field4 (Exp (a,b,c,d,e,f,g,h,i)) (Exp (a,b,c,d',e,f,g,h,i)) (Exp d) (Exp d') where
  _4 f = liftLens (L._4 :: Lens (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h, Exp i) (Exp a, Exp b, Exp c, Exp d', Exp e, Exp f, Exp g, Exp h, Exp i) (Exp d) (Exp d')) (fsink1 f)


-- Field5
-- ------

instance (Elt a, Elt b, Elt c, Elt d, Elt e, Elt e') => Field5 (Exp (a,b,c,d,e)) (Exp (a,b,c,d,e')) (Exp e) (Exp e') where
  _5 f = liftLens (L._5 :: Lens (Exp a, Exp b, Exp c, Exp d, Exp e) (Exp a, Exp b, Exp c, Exp d, Exp e') (Exp e) (Exp e')) (fsink1 f)

instance (Elt a, Elt b, Elt c, Elt d, Elt e, Elt e', Elt f) => Field5 (Exp (a,b,c,d,e,f)) (Exp (a,b,c,d,e',f)) (Exp e) (Exp e') where
  _5 f = liftLens (L._5 :: Lens (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f) (Exp a, Exp b, Exp c, Exp d, Exp e', Exp f) (Exp e) (Exp e')) (fsink1 f)

instance (Elt a, Elt b, Elt c, Elt d, Elt e, Elt e', Elt f, Elt g) => Field5 (Exp (a,b,c,d,e,f,g)) (Exp (a,b,c,d,e',f,g)) (Exp e) (Exp e') where
  _5 f = liftLens (L._5 :: Lens (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g) (Exp a, Exp b, Exp c, Exp d, Exp e', Exp f, Exp g) (Exp e) (Exp e')) (fsink1 f)

instance (Elt a, Elt b, Elt c, Elt d, Elt e, Elt e', Elt f, Elt g, Elt h) => Field5 (Exp (a,b,c,d,e,f,g,h)) (Exp (a,b,c,d,e',f,g,h)) (Exp e) (Exp e') where
  _5 f = liftLens (L._5 :: Lens (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h) (Exp a, Exp b, Exp c, Exp d, Exp e', Exp f, Exp g, Exp h) (Exp e) (Exp e')) (fsink1 f)

instance (Elt a, Elt b, Elt c, Elt d, Elt e, Elt e', Elt f, Elt g, Elt h, Elt i) => Field5 (Exp (a,b,c,d,e,f,g,h,i)) (Exp (a,b,c,d,e',f,g,h,i)) (Exp e) (Exp e') where
  _5 f = liftLens (L._5 :: Lens (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h, Exp i) (Exp a, Exp b, Exp c, Exp d, Exp e', Exp f, Exp g, Exp h, Exp i) (Exp e) (Exp e')) (fsink1 f)


-- Field6
-- ------

instance (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt f') => Field6 (Exp (a,b,c,d,e,f)) (Exp (a,b,c,d,e,f')) (Exp f) (Exp f') where
  _6 f = liftLens (L._6 :: Lens (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f) (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f') (Exp f) (Exp f')) (fsink1 f)

instance (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt f', Elt g) => Field6 (Exp (a,b,c,d,e,f,g)) (Exp (a,b,c,d,e,f',g)) (Exp f) (Exp f') where
  _6 f = liftLens (L._6 :: Lens (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g) (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f', Exp g) (Exp f) (Exp f')) (fsink1 f)

instance (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt f', Elt g, Elt h) => Field6 (Exp (a,b,c,d,e,f,g,h)) (Exp (a,b,c,d,e,f',g,h)) (Exp f) (Exp f') where
  _6 f = liftLens (L._6 :: Lens (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h) (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f', Exp g, Exp h) (Exp f) (Exp f')) (fsink1 f)

instance (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt f', Elt g, Elt h, Elt i) => Field6 (Exp (a,b,c,d,e,f,g,h,i)) (Exp (a,b,c,d,e,f',g,h,i)) (Exp f) (Exp f') where
  _6 f = liftLens (L._6 :: Lens (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h, Exp i) (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f', Exp g, Exp h, Exp i) (Exp f) (Exp f')) (fsink1 f)


-- Field7
-- ------

instance (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt g') => Field7 (Exp (a,b,c,d,e,f,g)) (Exp (a,b,c,d,e,f,g')) (Exp g) (Exp g') where
  _7 f = liftLens (L._7 :: Lens (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g) (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g') (Exp g) (Exp g')) (fsink1 f)

instance (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt g', Elt h) => Field7 (Exp (a,b,c,d,e,f,g,h)) (Exp (a,b,c,d,e,f,g',h)) (Exp g) (Exp g') where
  _7 f = liftLens (L._7 :: Lens (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h) (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g', Exp h) (Exp g) (Exp g')) (fsink1 f)

instance (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt g', Elt h, Elt i) => Field7 (Exp (a,b,c,d,e,f,g,h,i)) (Exp (a,b,c,d,e,f,g',h,i)) (Exp g) (Exp g') where
  _7 f = liftLens (L._7 :: Lens (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h, Exp i) (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g', Exp h, Exp i) (Exp g) (Exp g')) (fsink1 f)


-- Field8
-- ------

instance (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt h') => Field8 (Exp (a,b,c,d,e,f,g,h)) (Exp (a,b,c,d,e,f,g,h')) (Exp h) (Exp h') where
  _8 f = liftLens (L._8 :: Lens (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h) (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h') (Exp h) (Exp h')) (fsink1 f)

instance (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt h', Elt i) => Field8 (Exp (a,b,c,d,e,f,g,h,i)) (Exp (a,b,c,d,e,f,g,h',i)) (Exp h) (Exp h') where
  _8 f = liftLens (L._8 :: Lens (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h, Exp i) (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h', Exp i) (Exp h) (Exp h')) (fsink1 f)


-- Field9
-- ------

instance (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i, Elt i') => Field9 (Exp (a,b,c,d,e,f,g,h,i)) (Exp (a,b,c,d,e,f,g,h,i')) (Exp i) (Exp i') where
  _9 f = liftLens (L._9 :: Lens (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h, Exp i) (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h, Exp i') (Exp i) (Exp i')) (fsink1 f)



-- | Lift a 'Lens' into 'Exp' terms
--
liftLens
    :: (Functor f, Unlift Exp s, Unlift Exp t)
    => (l -> s -> f t)
    -> l
    -> Exp (Plain s)
    -> f (Exp (Plain t))
liftLens l f (unlift -> x) = lift <$> l f x


-- | Sink a unary functor from 'Exp'
--
fsink1 :: (Functor f, Unlift Exp b, Lift Exp a)
       => (Exp (Plain a) -> f (Exp (Plain b)))
       -> a
       -> f b
fsink1 f = fmap unlift . f . lift

