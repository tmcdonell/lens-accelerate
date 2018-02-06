{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Accelerate.Control.Lens.Shape
-- Copyright   : 2015 Trevor L. McDonell
-- License     : BSD-style (see the file LICENSE)
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable
--
----------------------------------------------------------------------------

module Data.Array.Accelerate.Control.Lens.Shape (

  module Control.Lens.Tuple

) where

import Control.Lens
import Control.Lens.Tuple
import Data.Array.Accelerate


instance (Slice sh, Elt a, Elt a')
    => Field1 (Exp (sh :. a)) (Exp (sh :. a')) (Exp a) (Exp a') where
  _1 = lens indexHead (\sh b -> lift (indexTail sh :. b))

instance (Slice sh, Elt a, Elt b, Elt b', Slice (sh :. b), Slice (sh :. b'))
    => Field2 (Exp (sh :. b :. a)) (Exp (sh :. b' :. a)) (Exp b) (Exp b') where
  _2 = lens (\s   -> let _  :. b :. _ = unlift s :: Exp sh :. Exp b :. Exp a in b)
            (\s b -> let sh :. _ :. a = unlift s :: Exp sh :. Exp b :. Exp a in lift (sh :. b :. a))

instance ( Slice sh, Elt a, Elt b, Elt c, Elt c'
         , Slice (sh :. c),  Slice (sh :. c  :. b)
         , Slice (sh :. c'), Slice (sh :. c' :. b)
         )
    => Field3 (Exp (sh :. c :. b :. a)) (Exp (sh :. c' :. b :. a)) (Exp c) (Exp c') where
  _3 = lens (\s   -> let _  :. c :. _ :. _ = unlift s :: Exp sh :. Exp c :. Exp b :. Exp a in c)
            (\s c -> let sh :. _ :. b :. a = unlift s :: Exp sh :. Exp c :. Exp b :. Exp a in lift (sh :. c :. b :. a))

instance ( Slice sh, Elt a, Elt b, Elt c, Elt d, Elt d'
         , Slice (sh :. d),  Slice (sh :. d  :. c), Slice (sh :. d  :. c :. b)
         , Slice (sh :. d'), Slice (sh :. d' :. c), Slice (sh :. d' :. c :. b)
         )
    => Field4 (Exp (sh :. d :. c :. b :. a)) (Exp (sh :. d' :. c :. b :. a)) (Exp d) (Exp d') where
  _4 = lens (\s   -> let _  :. d :. _ :. _ :. _ = unlift s :: Exp sh :. Exp d :. Exp c :. Exp b :. Exp a in d)
            (\s d -> let sh :. _ :. c :. b :. a = unlift s :: Exp sh :. Exp d :. Exp c :. Exp b :. Exp a in lift (sh :. d :. c :. b :. a))

instance ( Slice sh, Elt a, Elt b, Elt c, Elt d, Elt e, Elt e'
         , Slice (sh :. e),  Slice (sh :. e  :. d), Slice (sh :. e  :. d :. c), Slice (sh :. e  :. d :. c :. b)
         , Slice (sh :. e'), Slice (sh :. e' :. d), Slice (sh :. e' :. d :. c), Slice (sh :. e' :. d :. c :. b)
         )
    => Field5 (Exp (sh :. e :. d :. c :. b :. a)) (Exp (sh :. e' :. d :. c :. b :. a)) (Exp e) (Exp e') where
  _5 = lens (\s   -> let _  :. e :. _ :. _ :. _ :. _ = unlift s :: Exp sh :. Exp e :. Exp d :. Exp c :. Exp b :. Exp a in e)
            (\s e -> let sh :. _ :. d :. c :. b :. a = unlift s :: Exp sh :. Exp e :. Exp d :. Exp c :. Exp b :. Exp a in lift (sh :. e :. d :. c :. b :. a))

instance ( Slice sh, Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt f'
         , Slice (sh :. f),  Slice (sh :. f  :. e), Slice (sh :. f  :. e :. d), Slice (sh :. f  :. e :. d :. c), Slice (sh :. f  :. e :. d :. c :. b)
         , Slice (sh :. f'), Slice (sh :. f' :. e), Slice (sh :. f' :. e :. d), Slice (sh :. f' :. e :. d :. c), Slice (sh :. f' :. e :. d :. c :. b)
         )
    => Field6 (Exp (sh :. f :. e :. d :. c :. b :. a)) (Exp (sh :. f' :. e :. d :. c :. b :. a)) (Exp f) (Exp f') where
  _6 = lens (\s   -> let _  :. f :. _ :. _ :. _ :. _ :. _ = unlift s :: Exp sh :. Exp f :. Exp e :. Exp d :. Exp c :. Exp b :. Exp a in f)
            (\s f -> let sh :. _ :. e :. d :. c :. b :. a = unlift s :: Exp sh :. Exp f :. Exp e :. Exp d :. Exp c :. Exp b :. Exp a in lift (sh :. f :. e :. d :. c :. b :. a))

instance ( Slice sh , Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt g'
         , Slice (sh :. g),  Slice (sh :. g  :. f), Slice (sh :. g  :. f :. e), Slice (sh :. g  :. f :. e :. d), Slice (sh :. g  :. f :. e :. d :. c), Slice (sh :. g  :. f :. e :. d :. c :. b)
         , Slice (sh :. g'), Slice (sh :. g' :. f), Slice (sh :. g' :. f :. e), Slice (sh :. g' :. f :. e :. d), Slice (sh :. g' :. f :. e :. d :. c), Slice (sh :. g' :. f :. e :. d :. c :. b)
         )
    => Field7 (Exp (sh :. g :. f :. e :. d :. c :. b :. a)) (Exp (sh :. g' :. f :. e :. d :. c :. b :. a)) (Exp g) (Exp g') where
  _7 = lens (\s   -> let _  :. g :. _ :. _ :. _ :. _ :. _ :. _ = unlift s :: Exp sh :. Exp g :. Exp f :. Exp e :. Exp d :. Exp c :. Exp b :. Exp a in g)
            (\s g -> let sh :. _ :. f :. e :. d :. c :. b :. a = unlift s :: Exp sh :. Exp g :. Exp f :. Exp e :. Exp d :. Exp c :. Exp b :. Exp a in lift (sh :. g :. f :. e :. d :. c :. b :. a))

instance ( Slice sh, Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt h'
         , Slice (sh :. h),  Slice (sh :. h  :. g), Slice (sh :. h  :. g :. f), Slice (sh :. h  :. g :. f :. e), Slice (sh :. h  :. g :. f :. e :. d), Slice (sh :. h  :. g :. f :. e :. d :. c), Slice (sh :. h  :. g :. f :. e :. d :. c :. b)
         , Slice (sh :. h'), Slice (sh :. h' :. g), Slice (sh :. h' :. g :. f), Slice (sh :. h' :. g :. f :. e), Slice (sh :. h' :. g :. f :. e :. d), Slice (sh :. h' :. g :. f :. e :. d :. c), Slice (sh :. h' :. g :. f :. e :. d :. c :. b)
         )
    => Field8 (Exp (sh :. h :. g :. f :. e :. d :. c :. b :. a)) (Exp (sh :. h' :. g :. f :. e :. d :. c :. b :. a)) (Exp h) (Exp h') where
  _8 = lens (\s   -> let _  :. h :. _ :. _ :. _ :. _ :. _ :. _ :. _ = unlift s :: Exp sh :. Exp h :. Exp g :. Exp f :. Exp e :. Exp d :. Exp c :. Exp b :. Exp a in h)
            (\s h -> let sh :. _ :. g :. f :. e :. d :. c :. b :. a = unlift s :: Exp sh :. Exp h :. Exp g :. Exp f :. Exp e :. Exp d :. Exp c :. Exp b :. Exp a in lift (sh :. h :. g :. f :. e :. d :. c :. b :. a))

instance ( Slice sh, Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i, Elt i'
         , Slice (sh :. i),  Slice (sh :. i  :. h), Slice (sh :. i  :. h  :. g), Slice (sh :. i  :. h  :. g :. f), Slice (sh :. i  :. h  :. g :. f :. e), Slice (sh :. i  :. h  :. g :. f :. e :. d), Slice (sh :. i  :. h  :. g :. f :. e :. d :. c), Slice (sh :. i  :. h  :. g :. f :. e :. d :. c :. b)
         , Slice (sh :. i'), Slice (sh :. i' :. h), Slice (sh :. i' :. h :. g), Slice (sh :. i' :. h :. g :. f), Slice (sh :. i' :. h :. g :. f :. e), Slice (sh :. i' :. h :. g :. f :. e :. d), Slice (sh :. i' :. h :. g :. f :. e :. d :. c), Slice (sh :. i' :. h :. g :. f :. e :. d :. c :. b)
         )
    => Field9 (Exp (sh :. i :. h :. g :. f :. e :. d :. c :. b :. a)) (Exp (sh :. i' :. h :. g :. f :. e :. d :. c :. b :. a)) (Exp i) (Exp i') where
  _9 = lens (\s   -> let _  :. i :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ = unlift s :: Exp sh :. Exp i :. Exp h :. Exp g :. Exp f :. Exp e :. Exp d :. Exp c :. Exp b :. Exp a in i)
            (\s i -> let sh :. _ :. h :. g :. f :. e :. d :. c :. b :. a = unlift s :: Exp sh :. Exp i :. Exp h :. Exp g :. Exp f :. Exp e :. Exp d :. Exp c :. Exp b :. Exp a in lift (sh :. i :. h :. g :. f :. e :. d :. c :. b :. a))

