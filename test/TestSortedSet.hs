{-# LANGUAGE BangPatterns #-}
module TestSortedSet where

-- import Prelude hiding ((.))
import qualified Data.AATree.SortedSet as S
import qualified Data.AATree.AATree as I
import qualified Data.List as L
import Control.Monad
import qualified Control.Category as C
import Data.Maybe

testProp :: (S.Set Int -> Bool) -> [Int] -> Bool
testProp prop xs = xs .$ L.nub
                      .$ (flip S.insertAll) S.empty
                      .$ prop

propCheckP0, propCheckP1, propCheckP2, propCheckP3, propCheckP4, propCheckP5 :: [Int] -> Bool
propCheckP0 = testProp I.prop0
propCheckP1 = testProp I.prop1
propCheckP2 = testProp I.prop2
propCheckP3 = testProp I.prop3
propCheckP4 = testProp I.prop4
propCheckP5 = testProp I.prop5
          
propInsertMember :: [Int] -> Bool
propInsertMember xs = L.all (\x -> S.member x set) xs'
  where
          !set = S.insertAll xs' S.empty
          !xs' = L.nub xs

(.$) :: a -> (a -> b) -> b
x .$ f = f x

propDeleteAfterInsertImpliesNotMember :: [Int] -> Bool
propDeleteAfterInsertImpliesNotMember xs =
        let !s1 = S.insertAll xs' S.empty
            !s2 = S.deleteAll xs' s1
        in L.all (\x -> S.notMember x s2) xs'
  where
        !xs' = L.nub xs

propSorted :: [Int] -> Bool
propSorted xs = compare (S.toList tr1) (xs' .$ L.sort) == EQ  
        where
                !tr1 = S.insertAll xs' S.empty
                !xs' = L.nub xs

propSortedAfterDeletes :: [Int] -> Bool
propSortedAfterDeletes xs = compare (S.toList tr2) orderedSeq == EQ
        where
                !xs' = L.nub xs
                !ys = L.drop (length xs' `div` 2) xs'
                !orderedSeq = L.sort (xs' L.\\ ys)
                
                !tr1 = S.insertAll xs' S.empty
                !tr2 = S.deleteAll ys tr1

propYieldsOrigin :: [Int] -> Bool
propYieldsOrigin xs = compare ys zs == EQ
   where
           !ys = S.toList . S.fromList $ xs'
           !zs = L.sort xs'
           !xs' = L.nub xs

propMaximum :: [Int] -> Bool
propMaximum [] = True
propMaximum xs = maxOfSet == maximum xs
  where
    Just maxOfSet = S.findMax $ S.fromList xs

propMinimum :: [Int] -> Bool
propMinimum [] = True
propMinimum xs = minOfSet == minimum xs
  where
    Just minOfSet = S.findMin $ S.fromList xs
-------------------------------------------------------
