module ATree where

import Data.List
import Test.HUnit

data ATree a b = Leaf a | Fork b (ATree a b) (ATree a b)
             deriving (Eq, Ord, Show)

-- sample values

t1  = Leaf 1
t2  = Fork 1 (Leaf 2) (Leaf 3)
t3  = Fork 1 (Fork 2 (Leaf 4) (Leaf 5)) (Leaf 3)
t4  = Fork 1 (Fork 2 (Leaf 4) (Leaf 5)) (Fork 3 (Leaf 6) (Leaf 7))
t4' = Fork 3 (Fork 4 (Leaf 6) (Leaf 7)) (Fork 5 (Leaf 8) (Leaf 9))

-- explicit recursion

leavesR              :: ATree a b -> Int
leavesR (Leaf _)     = 1
leavesR (Fork _ l r) = leavesR l + leavesR r

testsATreeLeaves f =
  TestList 
  [ TestLabel "ATreeLeavest1" (TestCase (1 @=? f t1))
  , TestLabel "ATreeLeavest2" (TestCase (2 @=? f t2))
  , TestLabel "ATreeLeavest3" (TestCase (3 @=? f t3))
  , TestLabel "ATreeLeavest4" (TestCase (4 @=? f t4))
  ]

testsATreeLeavesR = testsATreeLeaves leavesR

forksR              :: ATree a b -> Int
forksR (Leaf _)     = 0
forksR (Fork _ l r) = 1 + forksR l + forksR r

testsATreeForks f =
  TestList 
  [ TestLabel "ATreeForkst1" (TestCase (0 @=? f t1))
  , TestLabel "ATreeForkst2" (TestCase (1 @=? f t2))
  , TestLabel "ATreeForkst3" (TestCase (2 @=? f t3))
  , TestLabel "ATreeForkst4" (TestCase (3 @=? f t4))
  ]
  
testsATreeForksR = testsATreeForks forksR
  
heightR              :: ATree a b -> Int
heightR (Leaf _)     = 0
heightR (Fork _ l r) = 1 + (heightR l `max` heightR r)

testsATreeHeight height =
  TestList 
  [ TestLabel "ATreeHeightt1" (TestCase (0 @=? height t1))
  , TestLabel "ATreeHeightt2" (TestCase (1 @=? height t2))
  , TestLabel "ATreeHeightt3" (TestCase (2 @=? height t3))
  , TestLabel "ATreeHeightt4" (TestCase (2 @=? height t4))
  ]
  
testsATreeHeightR = testsATreeHeight heightR

mapR                  :: (a -> c) -> (b -> d) -> ATree a b -> ATree c d
mapR f g (Leaf x)     = Leaf (f x)
mapR f g (Fork y l r) = Fork (g y) (mapR f g l) (mapR f g r)

testsATreeMap map = TestLabel "ATreeMap" (TestCase (t4' @=? map (+2) (+2) t4))
    
testsATreeMapR = testsATreeMap mapR

-- catamorphisms

fold              :: (a -> c) -> (b -> c -> c -> c) -> ATree a b -> c
fold f g (Leaf x) = f x
fold f g (Fork y l r) = g y (fold f g l) (fold f g r)

leaves :: ATree a b -> Int
leaves = fold (const 1) (const (+))
--                      (\_ u v -> u + v)

forks :: ATree a b -> Int
forks = fold (const 0) (const ((succ .) . (+)))
--                     (\_ u v -> 1 + u + v)

height :: ATree a b -> Int
height = fold (const 0) (const ((succ .) . max))
--                     (\_ u v -> 1 + (u `max` v))

mapCata :: (a -> c) -> (b -> d) -> ATree a b -> ATree c d
mapCata f g = fold (Leaf . f) (Fork . g)

fringe :: ATree a b -> [a]
fringe = fold (:[]) (const (++))
--            (\x -> [x]) (\_ u v -> u ++ v)

testsATreeLeavesCata = testsATreeLeaves leaves
testsATreeForksCata  = testsATreeForks forks
testsATreeHeightCata = testsATreeHeight height
testsATreeMapCata    = testsATreeMap mapCata

t4'' = 
  Fork 1 
    (Leaf 4)
    (Fork 2 
      (Fork 3 
        (Leaf 5)
        (Leaf 6)
      )
      (Leaf 7)
    )

testsATreeFringe = TestLabel "ATreeFringe" (TestCase (fringe t4'' @=? fringe t4))

-- anamorphisms
-- The Either type works here because there are only two kinds of nodes!
-- For domain-specific trees, we need to define a type with branches
-- corresponding to the tree algebra.

unfold :: (c -> Either a (b, c, c)) -> c -> ATree a b
unfold g z = case g z of
  Left x          -> Leaf x
  Right (w, y, z) -> Fork w (unfold g y) (unfold g z)

mapAna   :: (a -> c) -> (b -> d) -> ATree a b -> ATree c d
mapAna f g = unfold h where
  h (Leaf x)     = Left (f x)
  h (Fork w y z) = Right (g w, y, z)

testsATreeMapAna  = testsATreeMap mapAna

mkTree        :: Int -> ATree Int Int
mkTree leaves = unfold g 1 where
  g m | m > (leaves - 1) = Left m
  g m | m > 0            = Right (m, 2 * m, 2 * m + 1)

testsATreeMkTree = TestLabel "ATreeMkTree" (TestCase (t4 @=? mkTree 4))

depths :: ATree a b -> ATree Int Int
depths = down 0

down                :: Int -> ATree a b -> ATree Int Int
down n (Leaf _)     = Leaf n
down n (Fork _ l r) = Fork n (down (n + 1) l) (down (n + 1) r)

--exercise: what does down do?
--exercise: can you write "down" as a map? as a fold? as an unfold?

flattenR :: ATree a b -> [Either a b]
--flattenR = fold ((:[]) . Left) (\w l r -> Right w : l ++ r)
flattenR = fold ((:[]) . Left) (((++) .) . (:) . Right)

testsATreeFlatten flatten =
  TestList
  [ TestLabel "ATreeFlatten1" (TestCase ([1,2,4,5,3,6,7] @=? (map (either id id) (flatten t4))))
  , TestLabel "ATreeFlatten2" (TestCase ([1,4,2,3,5,6,7] @=? (map (either id id) (flatten t4''))))
  ]
  
testsATreeFlattenR = testsATreeFlatten flattenR  

--these two accessors are adapted from Data.Tree

rootLabel :: ATree a b -> Either a b
rootLabel (Leaf x)     = Left x
rootLabel (Fork w _ _) = Right w

subForest :: ATree a b -> [ATree a b]
subForest (Leaf _)     = []
subForest (Fork _ l r) = [l, r]

--literally from Data.Tree

levels   :: ATree a b -> [[Either a b]]
levels t = 
  map (map rootLabel) $
        takeWhile (not . null) $
        iterate (concatMap subForest) [t]

--breadth :: ATree a b -> Int
--exercise
--hint: start with levels and perform two more simple steps
--then reenable test below

testsATreeBreadth breadth =
  TestList
  [ TestLabel "ATreeBreadth1" (TestCase (4 @=? breadth t4))
  , TestLabel "ATreeBreadth2" (TestCase (2 @=? breadth t4''))
  ]

testsATreeAll =
  TestList
  [ testsATreeLeavesR
  , testsATreeForksR
  , testsATreeHeightR
  , testsATreeMapR
  , testsATreeLeavesCata
  , testsATreeForksCata  
  , testsATreeHeightCata  
  , testsATreeMapCata  
  , testsATreeFringe
  , testsATreeMapAna
  , testsATreeMkTree
  , testsATreeFlattenR
--  , testsATreeBreadth
  ]
