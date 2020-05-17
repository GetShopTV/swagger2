{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Swagger.Internal.TypeShape where

import Data.Proxy
import GHC.Generics
import GHC.TypeLits
import GHC.Exts (Constraint)

-- | Shape of a datatype.
data TypeShape
  = Enumeration     -- ^ A simple enumeration.
  | SumOfProducts   -- ^ A product or a sum of non-unit products.
  | Mixed           -- ^ Mixed sum type with both unit and non-unit constructors.

-- | A combined shape for a product type.
type family ProdCombine (a :: TypeShape) (b :: TypeShape) :: TypeShape where
  ProdCombine Mixed b     = Mixed   -- technically this cannot happen since Haskell types are sums of products
  ProdCombine a     Mixed = Mixed   -- technically this cannot happen since Haskell types are sums of products
  ProdCombine a     b     = SumOfProducts

-- | A combined shape for a sum type.
type family SumCombine (a :: TypeShape) (b :: TypeShape) :: TypeShape where
  SumCombine Enumeration   Enumeration   = Enumeration
  SumCombine SumOfProducts SumOfProducts = SumOfProducts
  SumCombine a b = Mixed

type family TypeHasSimpleShape t (f :: Symbol) :: Constraint where
  TypeHasSimpleShape t f = GenericHasSimpleShape t f (GenericShape (Rep t))

type family GenericHasSimpleShape t (f :: Symbol) (s :: TypeShape) :: Constraint where
  GenericHasSimpleShape t f Enumeration   = ()
  GenericHasSimpleShape t f SumOfProducts = ()
  GenericHasSimpleShape t f Mixed =
    TypeError
      (     Text "Cannot derive Generic-based Swagger Schema for " :<>: ShowType t
      :$$:  ShowType t :<>: Text " is a mixed sum type (has both unit and non-unit constructors)."
      :$$:  Text "Swagger does not have a good representation for these types."
      :$$:  Text "Use " :<>: Text f :<>: Text " if you want to derive schema"
      :$$:  Text "that matches aeson's Generic-based toJSON,"
      :$$:  Text "but that's not supported by some Swagger tools."
      )

-- | Infer a 'TypeShape' for a generic representation of a type.
type family GenericShape (g :: * -> *) :: TypeShape

type instance GenericShape (f :*: g)        = ProdCombine  (GenericShape f) (GenericShape g)
type instance GenericShape (f :+: g)        = SumCombine   (GenericShape f) (GenericShape g)
type instance GenericShape (D1  d f)        = GenericShape f
type instance GenericShape (C1 c U1)        = Enumeration
type instance GenericShape (C1 c (S1 s f))  = SumOfProducts
type instance GenericShape (C1 c (f :*: g)) = SumOfProducts
