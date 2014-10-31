{-# LANGUAGE PackageImports, TypeSynonymInstances, FlexibleInstances #-}
module Data.AATree.SortedSet (
  Set,
  empty,
  null,
  singleton,
  insert, insertAll,
  delete, deleteAll,
  member, notMember,
  toList, fromList,
  findMin, findMax,
) where

import Prelude hiding (null, empty, minimum, maximum)
import qualified Data.List as L
import Data.Monoid (Monoid, mempty, mappend)
import Data.Foldable (Foldable(..))
import qualified Data.Foldable as F

import Data.AATree.AATree as AA

type Set a = Tree a

insertAll :: (Ord k, Foldable t) => t k -> Set k -> Set k
insertAll xs set = F.foldl' (flip insert) set xs

deleteAll :: (Ord k, Foldable t) => t k -> Set k -> Set k
deleteAll xs set = F.foldl' (flip delete) set xs

-- | fromList O( n)
fromList :: (Ord k, Foldable t) => t k -> Set k
fromList xs = F.foldl' (flip insert) empty xs

notMember :: Ord k => k -> Set k -> Bool
notMember k = not . member k

unionL, unionR, union :: Ord k => Set k -> Set k -> Set k
unionR tx ty = L.foldl' (flip insert) tx (toList ty) -- on collision it keeps last inserted
unionL tx ty = L.foldl' (flip insert) ty (toList tx) -- on collision it keeps last inserted
union = unionL

instance (Ord a) => Monoid (Set a) where  -- requires extensions TypeSynonymInstances, FlexibleInstances
  mempty = empty
  mappend = union

  