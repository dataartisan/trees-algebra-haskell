module BTree where

import Test.HUnit

data BTree a = Leaf a | Fork (BTree a) (BTree a)
             deriving (Eq, Ord, Show)

-- sample values

t1  = Leaf 1
t2  = Fork (Leaf 2) (Leaf 3)
t3  = Fork (Fork (Leaf 4) (Leaf 5)) (Leaf 3)
t4  = Fork (Fork (Leaf 4) (Leaf 5)) (Fork (Leaf 6) (Leaf 7))
t4' = Fork (Fork (Leaf 6) (Leaf 7)) (Fork (Leaf 8) (Leaf 9))

-- explicit recursion

leavesR            :: BTree a -> Int
leavesR (Leaf _)   = 1
leavesR (Fork l r) = leavesR l + leavesR r

testsBTreeLeaves f =
  TestList 
  [ TestLabel "BTreeLeavest1" (TestCase (1 @=? f t1))
  , TestLabel "BTreeLeavest2" (TestCase (2 @=? f t2))
  , TestLabel "BTreeLeavest3" (TestCase (3 @=? f t3))
  , TestLabel "BTreeLeavest4" (TestCase (4 @=? f t4))
  ]

testsBTreeLeavesR = testsBTreeLeaves leavesR

forksR            :: BTree a -> Int
forksR (Leaf _)   = 0
forksR (Fork l r) = 1 + forksR l + forksR r

testsBTreeForks f =
  TestList 
  [ TestLabel "BTreeForkst1" (TestCase (0 @=? f t1))
  , TestLabel "BTreeForkst2" (TestCase (1 @=? f t2))
  , TestLabel "BTreeForkst3" (TestCase (2 @=? f t3))
  , TestLabel "BTreeForkst4" (TestCase (3 @=? f t4))
  ]
  
testsBTreeForksR = testsBTreeForks forksR
  
heightR            :: BTree a -> Int
heightR (Leaf _)   = 0
heightR (Fork l r) = 1 + (heightR l `max` heightR r)

testsBTreeHeight height =
  TestList 
  [ TestLabel "BTreeHeightt1" (TestCase (0 @=? height t1))
  , TestLabel "BTreeHeightt2" (TestCase (1 @=? height t2))
  , TestLabel "BTreeHeightt3" (TestCase (2 @=? height t3))
  , TestLabel "BTreeHeightt4" (TestCase (2 @=? height t4))
  ]
  
testsBTreeHeightR = testsBTreeHeight heightR

mapR              :: (a -> b) -> BTree a -> BTree b
mapR f (Leaf x)   = Leaf (f x)
mapR f (Fork l r) = Fork (mapR f l) (mapR f r)

testsBTreeMap map = TestLabel "BTreeMap" (TestCase (t4' @=? map (+2) t4))
    
testsBTreeMapR = testsBTreeMap mapR
                                          
-- catamorphisms

fold              :: (a -> b) -> (b -> b -> b) -> BTree a -> b
fold f g (Leaf x) = f x
fold f g (Fork l r) = g (fold f g l) (fold f g r)

leaves :: BTree a -> Int
leaves = fold (const 1) (+)

forks :: BTree a -> Int
forks = fold (const 0) (\l r -> 1 + l + r)

height :: BTree a -> Int
height = fold (const 0) (\l r -> 1 + (l `max` r))

mapCata :: (a -> b) -> BTree a -> BTree b
mapCata f = fold (Leaf . f) Fork

fringe :: BTree a -> [a]
fringe = fold (:[]) (++)

testsBTreeLeavesCata = testsBTreeLeaves leaves
testsBTreeForksCata  = testsBTreeForks forks
testsBTreeHeightCata = testsBTreeHeight height
testsBTreeMapCata    = testsBTreeMap mapCata

t4'' = 
  Fork 
    (Leaf 4)
    (Fork 
      (Fork 
        (Leaf 5)
        (Leaf 6)
      )
      (Leaf 7)
    )

testsBTreeFringe = TestLabel "BTreeFringe" (TestCase (fringe t4'' @=? fringe t4))

-- anamorphisms
-- The Either type works here because there are only two kinds of nodes!
-- For domain-specific trees, we need to define a type with branches
-- corresponding to the tree algebra.

unfold :: (a -> Either b (a, a)) -> a -> BTree b
unfold g z = case g z of
  Left x       -> Leaf x
  Right (y, z) -> Fork (unfold g y) (unfold g z)

mapAna   :: (a -> b) -> BTree a -> BTree b
mapAna f = unfold g where
  g (Leaf x)   = Left (f x)
  g (Fork y z) = Right (y, z)

testsBTreeMapAna  = testsBTreeMap mapAna

mkTree        :: Int -> BTree Int
mkTree leaves = unfold g 1 where
  g m | m > (leaves - 1) = Left m
  g m | m > 0            = Right (2 * m, 2 * m + 1)

testsBTreeMkTree = TestLabel "BTreeMkTree" (TestCase (t4 @=? mkTree 4))


testsBTreeAll =
  TestList
  [ testsBTreeLeavesR
  , testsBTreeForksR
  , testsBTreeHeightR
  , testsBTreeMapR
  , testsBTreeLeavesCata
  , testsBTreeForksCata  
  , testsBTreeHeightCata  
  , testsBTreeMapCata  
  , testsBTreeFringe
  , testsBTreeMapAna
  , testsBTreeMkTree
  ]
