{-# LANGUAGE PackageImports #-}
module Data.AATree.AATree (
  Tree,
  empty, singleton, null,
  insert, delete, member,
  toList,
  minimum, maximum,        
  checkProp, prop0, prop1, prop2, prop3, prop4, prop5,        
) where

import Prelude hiding (null, minimum, maximum)
import Control.Category ((>>>))
import qualified "dlist" Data.DList as D
import qualified Data.List as L


data Tree a = Nil | Node {value :: a, level :: Int, left, right :: Tree a} deriving (Show)

getLevel :: Tree a -> Int
getLevel Nil = 0
getLevel (Node _ lv _ _) = lv

(.$) :: a -> (a -> b) -> b
(.$) = flip ($)

empty = Nil

null Nil = True
null _ = False

singleton :: a -> Tree a
singleton x = Node x 1 Nil Nil

-- | right rotation
skew :: Tree a -> Tree a
skew t @ (Node _ lvT l @ (Node _ lvL a b) r)
  | lvT == lvL = let t' = t {left = right l}
                     l' = l {right = t'}
                 in l'
          
  | otherwise = t
skew t = t  

-- | left rotation and level increase
split :: Tree a -> Tree a
split t @ (Node _ lvT a r @ (Node _ lvR b x @ (Node _ lvX _ _)))
  | lvT == lvX = let t' = t {right = left r}
                     r' = r {left = t', level = lvR + 1}
                 in r'
  | otherwise = t
split t = t
  
insert, delete :: Ord a => a -> Tree a -> Tree a
insert x Nil = singleton x
insert x (Node y lv l r)
   | x < y = Node y lv (insert x l) r .$ skew .$ split
   | x > y = Node y lv l (insert x r) .$ skew .$ split
   | x == y = Node x lv l r

delete x Nil = Nil
delete x t @ (Node y lv l r)
   | x < y = Node y lv (delete x l) r .$ rebalance
   | x > y = Node y lv l (delete x r) .$ rebalance
   | x == y = case (l, r) of
                   (Nil, Nil) -> Nil -- deleted
                   (Nil, _) -> Node successor lv l (delete successor r) .$ rebalance -- copy successor value and delete successor
                        where successor = minimum r
                   (_, _) -> Node predecessor lv (delete predecessor l) r .$ rebalance -- copy predecessor value and delete predecessor
                        where predecessor = maximum l      

minimum (Node x _ Nil _) = x
minimum (Node x _ l _) = minimum l
minimum Nil = error "minimum: empty tree"

maximum (Node x _ _ Nil) = x
maximum (Node x _ _ r) = maximum r
maximum Nil = error "maximum: empty tree"
-----------------------------------------------------------------

rebalance :: Ord a => Tree a -> Tree a
rebalance = decreaseLevel >>> skew >>> skewRight >>> skewRightRight >>> split >>> splitRight

decreaseLevel :: Ord a => Tree a -> Tree a
decreaseLevel Nil = Nil

decreaseLevel t @ (Node _ lvP l Nil)
  | lvP > should_be = t {level = should_be}
  | otherwise = t
  where should_be = 1 + getLevel l

decreaseLevel t @ (Node _ lvP l r @ (Node _ lvR _ _))
  | lvP > should_be = let r' = if (r .$ level) > should_be
                                    then r {level = should_be}
                                    else r
                     in t {level = should_be, right = r'}
  | otherwise = t
        where should_be = 1 + min (getLevel l) (getLevel r)
              
-- decreaseLevel t = t
        
splitRight, skewRight, skewRightRight :: Tree a -> Tree a
splitRight Nil = Nil  
splitRight (Node x lv l r) = Node x lv l (split r)

skewRight Nil = Nil
skewRight (Node x lv l r) = Node x lv l (skew r)

skewRightRight (Node x lv l (Node x' lv' l' r')) = Node x lv l (Node x' lv' l' (skew r'))
skewRightRight t = t

-----------------------------------------------------------------

member :: Ord a => a -> Tree a -> Bool
member x Nil = False
member x (Node y _ l r)
        | x == y = True
        | x < y = member x l
        | otherwise = member x r


toDList :: Tree a -> D.DList a
toDList Nil = D.empty
toDList (Node x _ Nil Nil) = D.singleton x
toDList (Node x _ l r) = toDList l `D.append` D.singleton x `D.append` toDList r

toList :: Tree a -> [a]
toList = toDList >>> D.toList

-----------------------------------------------------------------

-- prop BST
-- all node keys from left tree are less than actual one.
-- all node keys from right tree are greter than
-- left and right trees also comply
prop0 :: Ord a => Tree a -> Bool
prop0 Nil = True
prop0 (Node x _ l r) = L.all (\y -> y < x) (toList l) &&
                       L.all (\y -> x < y) (toList r) &&
                       prop0 l && prop0 r

prop1, prop2, prop3, prop4, prop5 :: Tree a -> Bool

-- leaf nodes have level 1
prop1 (Node _ lv Nil Nil) = lv == 1
prop1 (Node _ _ l r) = prop1 l && prop1 r 
prop1 Nil = True

-- if there is a left child, the level of parent is one greater than the child one
prop2 (Node _ lvParent l @ (Node _ lvLChild _ _) r) = lvParent == 1 + lvLChild && prop2 l && prop2 r
prop2 (Node _ _ l r) = prop2 l && prop2 r
prop2 Nil = True

prop3 (Node _ lvParent l r @ (Node _ lvRChild _ _)) = lvParent - lvRChild <= 1 && prop3 l && prop3 r
prop3 (Node _ _ l r) = prop3 l && prop3 r
prop3 Nil = True

prop4 (Node _ lvParent l r @ (Node _ lvRChild _ (Node _ lvRGChild _ _))) = lvRGChild < lvParent && prop4 l && prop4 r
prop4 (Node _ _ l r) = prop4 l && prop4 r
prop4 Nil = True

-- all nodes with level > 1 have two children
prop5 (Node _ lv l r)
        | lv > 1 = (not . null $ l) && (not . null $ r) && prop5 l && prop5 r
        | otherwise = prop5 l && prop5 r
prop5 Nil = True

checkProp :: (Tree a -> Bool) -> Tree a -> Bool
checkProp prop Nil = True
checkProp prop t @ (Node _ _ l r) = prop t && checkProp prop l && checkProp prop r





