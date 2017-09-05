
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}


module Data.Swagger.Internal.TypeShape where


import Data.Proxy
import GHC.Generics
import GHC.TypeLits
 
data TypeShape = EnumShape
               | NestedShape
               | IlegalShape


type family ProdCombine (a :: TypeShape ) (b :: TypeShape) :: TypeShape where
        
        ProdCombine IlegalShape b           = IlegalShape
        ProdCombine a           IlegalShape = IlegalShape
        ProdCombine a           b           = NestedShape


type family SumCombine (a :: TypeShape ) (b :: TypeShape) :: TypeShape where
        
        SumCombine  EnumShape   EnumShape   = EnumShape
        SumCombine  NestedShape NestedShape = NestedShape
        SumCombine  a           b           = IlegalShape




class LegalShape (a :: TypeShape) where

instance LegalShape EnumShape
instance LegalShape NestedShape

instance TypeError 
       (    Text "Cannot auto derive swagger class, for that to be possible, make sure that any constructor " 
       :$$: Text "for this type, only holds 0 arguments if there are no other contructors with more than 0 arguments,"
       :$$: Text "otherwise, every constructor have to hold at least 1 argument"    
       ) => LegalShape IlegalShape



type family GenericShape ( g :: * -> * ) :: TypeShape


type instance GenericShape (f :*: g)        = ProdCombine  (GenericShape f) (GenericShape g)
type instance GenericShape (f :+: g)        = SumCombine   (GenericShape f) (GenericShape g)
type instance GenericShape (D1  d f)        = GenericShape f
type instance GenericShape (C1 c U1)        = EnumShape
type instance GenericShape (C1 c (S1 s f))  = NestedShape
type instance GenericShape (C1 c (f :*: g)) = NestedShape


