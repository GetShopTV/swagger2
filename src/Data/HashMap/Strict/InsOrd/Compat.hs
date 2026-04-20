{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.HashMap.Strict.InsOrd.Compat (
  InsOrdHashMap,
  -- * Construction
  empty,
  singleton,
  -- * Basic interface
  null,
  size,
  member,
  lookup,
  lookupDefault,
  insert,
  insertWith,
  delete,
  adjust,
  update,
  alter,
  -- * Combine
  union,
  unionWith,
  unionWithKey,
  unions,
  -- * Transformations
  map,
  mapKeys,
  traverseKeys,
  mapWithKey,
  traverseWithKey,
  -- ** Unordered
  unorderedTraverse,
  unorderedTraverseWithKey,
  -- * Difference and intersection
  difference,
  intersection,
  intersectionWith,
  intersectionWithKey,
  -- * Folds
  foldl',
  foldlWithKey',
  foldr,
  foldrWithKey,
  foldMapWithKey,
  -- ** Unordered
  unorderedFoldMap,
  unorderedFoldMapWithKey,
  -- * Filter
  filter,
  filterWithKey,
  mapMaybe,
  mapMaybeWithKey,
  -- * Conversions
  keys,
  elems,
  toList,
  toRevList,
  fromList,
  toHashMap,
  fromHashMap,
  -- -- * Lenses
  -- hashMap,
  -- unorderedTraversal,
  -- -- * Debugging
  -- valid,
  ) where

#if !MIN_VERSION_insert_ordered_containers(0,3,0)
import Data.HashMap.Strict.InsOrd
#else
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap

import Prelude hiding (null, size, member, lookup, lookupDefault, map, foldl', filter)
import qualified Prelude

import qualified Data.Aeson as A
import qualified Data.Aeson.Encoding as E
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

import qualified GHC.Exts as Exts

import Data.Data                       (Data)
import Data.Foldable                   (Foldable (foldMap))
import Data.Hashable                   (Hashable (..))

import qualified Control.Lens        as Lens
import Control.Lens
       (At (..), Index, Iso, IxValue, Ixed (..), Traversal, _1, _2, iso, (<&>))

import qualified Optics.Core         as Optics

newtype InsOrdHashMap k v = InsOrdHashMap { unCompatInsOrdHashMap :: InsOrdHashMap.InsOrdHashMap k v }
  deriving (Show, Read, Data, Functor, Foldable, Traversable, Semigroup, Monoid)

instance (Eq k, Eq v) => Eq (InsOrdHashMap k v) where
    a == b = toHashMap a == toHashMap b

instance (Eq k, Hashable k) => Exts.IsList (InsOrdHashMap k v) where
    type Item (InsOrdHashMap k v) = Exts.Item (InsOrdHashMap.InsOrdHashMap k v)
    fromList = InsOrdHashMap . InsOrdHashMap.fromList
    toList   = InsOrdHashMap.toList . unCompatInsOrdHashMap

-------------------------------------------------------------------------------
-- Aeson
-------------------------------------------------------------------------------

instance (A.ToJSONKey k) => A.ToJSON1 (InsOrdHashMap k) where
    liftToJSON _ t _ = case A.toJSONKey :: A.ToJSONKeyFunction k of
      A.ToJSONKeyText f _ -> A.object . fmap (\(k, v) -> (f k, t v)) . toList
      A.ToJSONKeyValue f _ -> A.toJSON . fmap (\(k,v) -> A.toJSON (f k, t v)) . toList

    liftToEncoding o t _ = case A.toJSONKey :: A.ToJSONKeyFunction k of
      A.ToJSONKeyText _ f ->  E.dict f t foldrWithKey
      A.ToJSONKeyValue _ f -> E.list (A.liftToEncoding2 (const False) f (E.list f) o t (E.list t)) . toList

instance (A.ToJSONKey k, A.ToJSON v) => A.ToJSON (InsOrdHashMap k v) where
    toJSON = A.toJSON1
    toEncoding = A.toEncoding1

-------------------------------------------------------------------------------

instance (Eq k, Hashable k, A.FromJSONKey k) => A.FromJSON1 (InsOrdHashMap k) where
    liftParseJSON o p pl v = fromList . HashMap.toList <$> A.liftParseJSON o p pl v

instance (Eq k, Hashable k, A.FromJSONKey k, A.FromJSON v) => A.FromJSON (InsOrdHashMap k v) where
    parseJSON = A.parseJSON1

-------------------------------------------------------------------------------
-- indexed-traversals
-------------------------------------------------------------------------------

instance (Eq k, Hashable k) => Optics.FunctorWithIndex k (InsOrdHashMap k) where
    imap = mapWithKey
instance (Eq k, Hashable k) => Optics.FoldableWithIndex k (InsOrdHashMap k) where
    ifoldMap = foldMapWithKey
    ifoldr   = foldrWithKey
instance (Eq k, Hashable k) => Optics.TraversableWithIndex k (InsOrdHashMap k) where
    itraverse = traverseWithKey

-------------------------------------------------------------------------------
-- Lens
-------------------------------------------------------------------------------

type instance Index (InsOrdHashMap k v) = k
type instance IxValue (InsOrdHashMap k v) = v

instance (Eq k, Hashable k) => Ixed (InsOrdHashMap k v) where
    ix k f m = ixImpl k pure f m
    {-# INLINABLE ix #-}

ixImpl
  :: (Eq k, Hashable k, Functor f)
  => k
  -> (InsOrdHashMap k v -> f (InsOrdHashMap k v))
  -> (v -> f v)
  -> InsOrdHashMap k v
  -> f (InsOrdHashMap k v)
ixImpl k point f m = case lookup k m of
    Just v  -> f v <&> \v' -> insert k v' m
    Nothing -> point m
{-# INLINE ixImpl #-}

instance (Eq k, Hashable k) => At (InsOrdHashMap k a) where
    at k f m = f mv <&> \r -> case r of
        Nothing -> maybe m (const (delete k m)) mv
        Just v' -> insert k v' m
      where mv = lookup k m
    {-# INLINABLE at #-}

-------------------------------------------------------------------------------
-- Optics
-------------------------------------------------------------------------------

type instance Optics.Index (InsOrdHashMap k v) = k
type instance Optics.IxValue (InsOrdHashMap k v) = v

instance (Eq k, Hashable k) => Optics.Ixed (InsOrdHashMap k v) where
    ix k = Optics.atraversalVL $ \point f m -> ixImpl k point f m
    {-# INLINE ix #-}

instance (Eq k, Hashable k) => Optics.At (InsOrdHashMap k a) where
    at k = Optics.lensVL $ \f m -> Lens.at k f m
    {-# INLINE at #-}

-------------------------------------------------------------------------------

-- | This is a slight lie, as roundtrip doesn't preserve ordering.
hashMap :: Iso (InsOrdHashMap k a) (InsOrdHashMap k b) (HashMap k a) (HashMap k b)
hashMap = iso toHashMap fromHashMap

unorderedTraversal :: Traversal (InsOrdHashMap k a) (InsOrdHashMap k b) a b
unorderedTraversal = hashMap . traverse

-------------------------------------------------------------------------------
-- * Construction
-------------------------------------------------------------------------------

empty :: InsOrdHashMap k v
empty = InsOrdHashMap InsOrdHashMap.empty

singleton :: Hashable k => k -> v -> InsOrdHashMap k v
singleton k v = InsOrdHashMap (InsOrdHashMap.singleton k v)

-------------------------------------------------------------------------------
-- * Basic interface
-------------------------------------------------------------------------------

null :: InsOrdHashMap k v -> Bool
null = InsOrdHashMap.null . unCompatInsOrdHashMap

size :: InsOrdHashMap k v -> Int
size = InsOrdHashMap.size . unCompatInsOrdHashMap

insert :: Hashable k => k -> v -> InsOrdHashMap k v -> InsOrdHashMap k v
insert k v = InsOrdHashMap . InsOrdHashMap.insert k v . unCompatInsOrdHashMap

insertWith :: Hashable k => (v -> v -> v) -> k -> v -> InsOrdHashMap k v -> InsOrdHashMap k v
insertWith f k v = InsOrdHashMap . InsOrdHashMap.insertWith f k v . unCompatInsOrdHashMap

delete :: Hashable k => k -> InsOrdHashMap k v -> InsOrdHashMap k v
delete k = InsOrdHashMap . InsOrdHashMap.delete k . unCompatInsOrdHashMap

adjust :: Hashable k => (v -> v) -> k -> InsOrdHashMap k v -> InsOrdHashMap k v
adjust f k = InsOrdHashMap . InsOrdHashMap.adjust f k . unCompatInsOrdHashMap

update :: Hashable k => (v -> Maybe v) -> k -> InsOrdHashMap k v -> InsOrdHashMap k v
update f k = InsOrdHashMap . InsOrdHashMap.update f k . unCompatInsOrdHashMap

alter :: Hashable k => (Maybe v -> Maybe v) -> k -> InsOrdHashMap k v -> InsOrdHashMap k v
alter f k = InsOrdHashMap . InsOrdHashMap.alter f k . unCompatInsOrdHashMap

member :: Hashable k => k -> InsOrdHashMap k v -> Bool
member k = InsOrdHashMap.member k . unCompatInsOrdHashMap

lookup :: Hashable k => k -> InsOrdHashMap k v -> Maybe v
lookup k = InsOrdHashMap.lookup k . unCompatInsOrdHashMap

lookupDefault :: Hashable k => v -> k -> InsOrdHashMap k v -> v
lookupDefault k def = InsOrdHashMap.lookupDefault k def . unCompatInsOrdHashMap

-- * Combine

union :: Hashable k => InsOrdHashMap k v -> InsOrdHashMap k v -> InsOrdHashMap k v
union m1 m2 = InsOrdHashMap (InsOrdHashMap.union (unCompatInsOrdHashMap m1) (unCompatInsOrdHashMap m2))

unionWith :: Hashable k => (v -> v -> v) -> InsOrdHashMap k v -> InsOrdHashMap k v -> InsOrdHashMap k v
unionWith f m1 m2 = InsOrdHashMap (InsOrdHashMap.unionWith f (unCompatInsOrdHashMap m1) (unCompatInsOrdHashMap m2))

unionWithKey :: Hashable k => (k -> v -> v -> v) -> InsOrdHashMap k v -> InsOrdHashMap k v -> InsOrdHashMap k v
unionWithKey f m1 m2 = InsOrdHashMap (InsOrdHashMap.unionWithKey f (unCompatInsOrdHashMap m1) (unCompatInsOrdHashMap m2))

unions :: Hashable k => [InsOrdHashMap k v] -> InsOrdHashMap k v
unions = InsOrdHashMap . InsOrdHashMap.unions . Prelude.map unCompatInsOrdHashMap

-------------------------------------------------------------------------------
-- * Transformations
-------------------------------------------------------------------------------

map :: (v -> v) -> InsOrdHashMap k v -> InsOrdHashMap k v
map f = InsOrdHashMap . InsOrdHashMap.map f . unCompatInsOrdHashMap

mapKeys :: Hashable k => (k -> k) -> InsOrdHashMap k v -> InsOrdHashMap k v
mapKeys f = InsOrdHashMap . InsOrdHashMap.mapKeys f . unCompatInsOrdHashMap

traverseKeys :: (Applicative f, Hashable k) => (k -> f k) -> InsOrdHashMap k v -> f (InsOrdHashMap k v)
traverseKeys f = fmap InsOrdHashMap . InsOrdHashMap.traverseKeys f . unCompatInsOrdHashMap

mapWithKey :: (k -> v1 -> v2) -> InsOrdHashMap k v1 -> InsOrdHashMap k v2
mapWithKey f = InsOrdHashMap . InsOrdHashMap.mapWithKey f . unCompatInsOrdHashMap

traverseWithKey :: (Applicative f, Hashable k) => (k -> v1 -> f v2) -> InsOrdHashMap k v1 -> f (InsOrdHashMap k v2)
traverseWithKey f = fmap InsOrdHashMap . InsOrdHashMap.traverseWithKey f . unCompatInsOrdHashMap

-- ** Unordered

unorderedTraverse :: (Applicative f, Hashable k) => (v -> f v) -> InsOrdHashMap k v -> f (InsOrdHashMap k v)
unorderedTraverse f = fmap InsOrdHashMap . InsOrdHashMap.unorderedTraverse f . unCompatInsOrdHashMap

unorderedTraverseWithKey :: (Applicative f, Hashable k) => (k -> v -> f v) -> InsOrdHashMap k v -> f (InsOrdHashMap k v)
unorderedTraverseWithKey f = fmap InsOrdHashMap . InsOrdHashMap.unorderedTraverseWithKey f . unCompatInsOrdHashMap

-------------------------------------------------------------------------------
-- * Difference and intersection
-------------------------------------------------------------------------------

difference :: Hashable k => InsOrdHashMap k v -> InsOrdHashMap k v -> InsOrdHashMap k v
difference m1 m2 = InsOrdHashMap (InsOrdHashMap.difference (unCompatInsOrdHashMap m1) (unCompatInsOrdHashMap m2))

intersection :: Hashable k => InsOrdHashMap k v -> InsOrdHashMap k v -> InsOrdHashMap k v
intersection m1 m2 = InsOrdHashMap (InsOrdHashMap.intersection (unCompatInsOrdHashMap m1) (unCompatInsOrdHashMap m2))

intersectionWith :: Hashable k => (v -> v -> v) -> InsOrdHashMap k v -> InsOrdHashMap k v -> InsOrdHashMap k v
intersectionWith f m1 m2 = InsOrdHashMap (InsOrdHashMap.intersectionWith f (unCompatInsOrdHashMap m1) (unCompatInsOrdHashMap m2))

intersectionWithKey :: Hashable k => (k -> v -> v -> v) -> InsOrdHashMap k v -> InsOrdHashMap k v -> InsOrdHashMap k v
intersectionWithKey f m1 m2 = InsOrdHashMap (InsOrdHashMap.intersectionWithKey f (unCompatInsOrdHashMap m1) (unCompatInsOrdHashMap m2))

-------------------------------------------------------------------------------
-- * Folds
-------------------------------------------------------------------------------

foldl' :: (a -> v -> a) -> a -> InsOrdHashMap k v -> a
foldl' f z = InsOrdHashMap.foldl' f z . unCompatInsOrdHashMap

foldlWithKey' :: (a -> k -> v -> a) -> a -> InsOrdHashMap k v -> a
foldlWithKey' f z = InsOrdHashMap.foldlWithKey' f z . unCompatInsOrdHashMap

foldMapWithKey :: Monoid m => (k -> v -> m) -> InsOrdHashMap k v -> m
foldMapWithKey f = InsOrdHashMap.foldMapWithKey f . unCompatInsOrdHashMap

foldrWithKey :: (k -> v -> a -> a) -> a -> InsOrdHashMap k v -> a
foldrWithKey f z = InsOrdHashMap.foldrWithKey f z . unCompatInsOrdHashMap

-- ** Unordered

unorderedFoldMap :: Monoid m => (v -> m) -> InsOrdHashMap k v -> m
unorderedFoldMap f = InsOrdHashMap.unorderedFoldMap f . unCompatInsOrdHashMap

unorderedFoldMapWithKey :: Monoid m => (k -> v -> m) -> InsOrdHashMap k v -> m
unorderedFoldMapWithKey f = InsOrdHashMap.unorderedFoldMapWithKey f . unCompatInsOrdHashMap

-------------------------------------------------------------------------------
-- * Filter
-------------------------------------------------------------------------------

filter :: (v -> Bool) -> InsOrdHashMap k v -> InsOrdHashMap k v
filter f = InsOrdHashMap . InsOrdHashMap.filter f . unCompatInsOrdHashMap

filterWithKey :: (k -> v -> Bool) -> InsOrdHashMap k v -> InsOrdHashMap k v
filterWithKey f = InsOrdHashMap . InsOrdHashMap.filterWithKey f . unCompatInsOrdHashMap

mapMaybe :: (v -> Maybe v) -> InsOrdHashMap k v -> InsOrdHashMap k v
mapMaybe f = InsOrdHashMap . InsOrdHashMap.mapMaybe f . unCompatInsOrdHashMap

mapMaybeWithKey :: (k -> v -> Maybe v) -> InsOrdHashMap k v -> InsOrdHashMap k v
mapMaybeWithKey f = InsOrdHashMap . InsOrdHashMap.mapMaybeWithKey f . unCompatInsOrdHashMap

-------------------------------------------------------------------------------
-- * Conversions
-------------------------------------------------------------------------------

keys :: InsOrdHashMap k v -> [k]
keys = InsOrdHashMap.keys . unCompatInsOrdHashMap

elems :: InsOrdHashMap k v -> [v]
elems = InsOrdHashMap.elems . unCompatInsOrdHashMap

toRevList :: InsOrdHashMap k v -> [(k, v)]
toRevList = InsOrdHashMap.toRevList . unCompatInsOrdHashMap

fromList :: Hashable k => [(k, v)] -> InsOrdHashMap k v
fromList = InsOrdHashMap . InsOrdHashMap.fromList

toList :: InsOrdHashMap k v -> [(k, v)]
toList = InsOrdHashMap.toList . unCompatInsOrdHashMap

toHashMap :: InsOrdHashMap k v -> HashMap k v
toHashMap = InsOrdHashMap.toHashMap . unCompatInsOrdHashMap

fromHashMap :: HashMap k v -> InsOrdHashMap k v
fromHashMap = InsOrdHashMap . InsOrdHashMap.fromHashMap

#endif