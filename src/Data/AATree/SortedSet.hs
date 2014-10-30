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
  minimum, maximum,        
) where

import Prelude hiding (null, empty, minimum, maximum)
-- import Data.Monoid (Monoid, mempty, mappend, (<>), mconcat)
import Data.Foldable (Foldable(..))
import qualified Data.Foldable as F

import Data.AATree.AATree

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

