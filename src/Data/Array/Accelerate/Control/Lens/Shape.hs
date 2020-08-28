{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Accelerate.Control.Lens.Shape
-- Copyright   : [2015..2020] Trevor L. McDonell
-- License     : BSD-style (see the file LICENSE)
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
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


instance (Elt sh, Elt a, Elt a')
    => Field1 (Exp (sh :. a)) (Exp (sh :. a')) (Exp a) (Exp a') where
  _1 = lens indexHead (\sh b -> lift (indexTail sh :. b))

instance (Elt sh, Elt a, Elt b, Elt b')
    => Field2 (Exp (sh :. b :. a)) (Exp (sh :. b' :. a)) (Exp b) (Exp b') where
  _2 = lens (\s   -> let _  :. b :. _ = unlift s :: Exp sh :. Exp b :. Exp a in b)
            (\s b -> let sh :. _ :. a = unlift s :: Exp sh :. Exp b :. Exp a in lift (sh :. b :. a))

instance (Elt sh, Elt a, Elt b, Elt c, Elt c')
    => Field3 (Exp (sh :. c :. b :. a)) (Exp (sh :. c' :. b :. a)) (Exp c) (Exp c') where
  _3 = lens (\s   -> let _  :. c :. _ :. _ = unlift s :: Exp sh :. Exp c :. Exp b :. Exp a in c)
            (\s c -> let sh :. _ :. b :. a = unlift s :: Exp sh :. Exp c :. Exp b :. Exp a in lift (sh :. c :. b :. a))

instance (Elt sh, Elt a, Elt b, Elt c, Elt d, Elt d')
    => Field4 (Exp (sh :. d :. c :. b :. a)) (Exp (sh :. d' :. c :. b :. a)) (Exp d) (Exp d') where
  _4 = lens (\s   -> let _  :. d :. _ :. _ :. _ = unlift s :: Exp sh :. Exp d :. Exp c :. Exp b :. Exp a in d)
            (\s d -> let sh :. _ :. c :. b :. a = unlift s :: Exp sh :. Exp d :. Exp c :. Exp b :. Exp a in lift (sh :. d :. c :. b :. a))

instance (Elt sh, Elt a, Elt b, Elt c, Elt d, Elt e, Elt e')
    => Field5 (Exp (sh :. e :. d :. c :. b :. a)) (Exp (sh :. e' :. d :. c :. b :. a)) (Exp e) (Exp e') where
  _5 = lens (\s   -> let _  :. e :. _ :. _ :. _ :. _ = unlift s :: Exp sh :. Exp e :. Exp d :. Exp c :. Exp b :. Exp a in e)
            (\s e -> let sh :. _ :. d :. c :. b :. a = unlift s :: Exp sh :. Exp e :. Exp d :. Exp c :. Exp b :. Exp a in lift (sh :. e :. d :. c :. b :. a))

instance (Elt sh, Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt f')
    => Field6 (Exp (sh :. f :. e :. d :. c :. b :. a)) (Exp (sh :. f' :. e :. d :. c :. b :. a)) (Exp f) (Exp f') where
  _6 = lens (\s   -> let _  :. f :. _ :. _ :. _ :. _ :. _ = unlift s :: Exp sh :. Exp f :. Exp e :. Exp d :. Exp c :. Exp b :. Exp a in f)
            (\s f -> let sh :. _ :. e :. d :. c :. b :. a = unlift s :: Exp sh :. Exp f :. Exp e :. Exp d :. Exp c :. Exp b :. Exp a in lift (sh :. f :. e :. d :. c :. b :. a))

instance (Elt sh , Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt g')
    => Field7 (Exp (sh :. g :. f :. e :. d :. c :. b :. a)) (Exp (sh :. g' :. f :. e :. d :. c :. b :. a)) (Exp g) (Exp g') where
  _7 = lens (\s   -> let _  :. g :. _ :. _ :. _ :. _ :. _ :. _ = unlift s :: Exp sh :. Exp g :. Exp f :. Exp e :. Exp d :. Exp c :. Exp b :. Exp a in g)
            (\s g -> let sh :. _ :. f :. e :. d :. c :. b :. a = unlift s :: Exp sh :. Exp g :. Exp f :. Exp e :. Exp d :. Exp c :. Exp b :. Exp a in lift (sh :. g :. f :. e :. d :. c :. b :. a))

instance (Elt sh, Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt h')
    => Field8 (Exp (sh :. h :. g :. f :. e :. d :. c :. b :. a)) (Exp (sh :. h' :. g :. f :. e :. d :. c :. b :. a)) (Exp h) (Exp h') where
  _8 = lens (\s   -> let _  :. h :. _ :. _ :. _ :. _ :. _ :. _ :. _ = unlift s :: Exp sh :. Exp h :. Exp g :. Exp f :. Exp e :. Exp d :. Exp c :. Exp b :. Exp a in h)
            (\s h -> let sh :. _ :. g :. f :. e :. d :. c :. b :. a = unlift s :: Exp sh :. Exp h :. Exp g :. Exp f :. Exp e :. Exp d :. Exp c :. Exp b :. Exp a in lift (sh :. h :. g :. f :. e :. d :. c :. b :. a))

instance (Elt sh, Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i, Elt i')
    => Field9 (Exp (sh :. i :. h :. g :. f :. e :. d :. c :. b :. a)) (Exp (sh :. i' :. h :. g :. f :. e :. d :. c :. b :. a)) (Exp i) (Exp i') where
  _9 = lens (\s   -> let _  :. i :. _ :. _ :. _ :. _ :. _ :. _ :. _ :. _ = unlift s :: Exp sh :. Exp i :. Exp h :. Exp g :. Exp f :. Exp e :. Exp d :. Exp c :. Exp b :. Exp a in i)
            (\s i -> let sh :. _ :. h :. g :. f :. e :. d :. c :. b :. a = unlift s :: Exp sh :. Exp i :. Exp h :. Exp g :. Exp f :. Exp e :. Exp d :. Exp c :. Exp b :. Exp a in lift (sh :. i :. h :. g :. f :. e :. d :. c :. b :. a))

