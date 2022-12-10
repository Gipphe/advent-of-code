module Y2022.Day8
    ( someDay8
    , computeTask1
    , computeTask2
    , input
    ) where

import Data.List (intercalate, singleton)
import Data.Monoid (Product(..), Sum(..))
import Data.Semigroup (Max(..))
import qualified Data.Vector as V
import Data.Vector (Vector)
import Day (Day, SomeDay(..), Task, runTask)
import Y2022.Input (day8Input)

input :: Forest
input =
    Forest
        . V.fromList
        . fmap V.fromList
        . fmap (fmap (Tree Hidden . read . singleton))
        . lines
        $ day8Input

someDay8 :: SomeDay
someDay8 = SomeDay day8

day8 :: Day 8 ()
day8 = do
    runTask day8Task1
    runTask day8Task2

day8Task1 :: Task 1 Int
day8Task1 = pure $ computeTask1 input

day8Task2 :: Task 2 Int
day8Task2 = pure $ computeTask2 input

computeTask1 :: Forest -> Int
computeTask1 =
    getSum
        . foldMap (Sum . length)
        . V.filter (not . V.null)
        . fmap (V.filter isVisible)
        . getForest
        . addVisibilities

computeTask2 :: Forest -> Int
computeTask2 =
    getMax
        . foldMap (foldMap (Max . scenicScore))
        . getScenicForest
        . addScenicScores

addVisibilities :: Forest -> Forest
addVisibilities forest = mapForest
    (V.imap (\y -> V.imap (\x -> evalTreeVisibility (Pos { y, x }) forest)))
    forest

evalTreeVisibility :: Pos -> Forest -> Tree -> Tree
evalTreeVisibility (Pos { y, x }) forest' tree
    | any V.null treeLines = setVisibility Visible tree
    | treeIsVisible        = setVisibility Visible tree
    | otherwise            = setVisibility Hidden tree
  where
    forest                    = getForest forest'
    row                       = forest V.! y
    column                    = (V.! x) <$> forest
    (treesToTheLeft, treesToTheRight') = safeSplitAt x row
    treesToTheRight           = safeTail treesToTheRight'
    (treesAbove, treesBelow') = safeSplitAt y column
    treesBelow                = safeTail treesBelow'
    treeLines = [treesToTheLeft, treesToTheRight, treesAbove, treesBelow]
    treeIsVisible             = or $ all (tree `isVisiblePast`) <$> treeLines

addScenicScores :: Forest -> ScenicForest
addScenicScores (Forest forest) = ScenicForest $ V.imap
    (\y -> V.imap (\x -> evalScenicTree (Pos { y, x }) forest))
    forest

evalScenicTree :: Pos -> Vector (Vector Tree) -> Tree -> ScenicTree
evalScenicTree (Pos { y, x }) forest tree = ScenicTree score tree
  where
    row                       = forest V.! y
    column                    = (V.! x) <$> forest
    (treesToTheLeft, treesToTheRight') = safeSplitAt x row
    treesToTheRight           = safeTail treesToTheRight'
    (treesAbove, treesBelow') = safeSplitAt y column
    treesBelow                = safeTail treesBelow'
    treesVisibleToTheLeft =
        viewingDistance tree $ reverse $ V.toList treesToTheLeft
    treesVisibleToTheRight = viewingDistance tree $ V.toList treesToTheRight
    treesVisibleAbove = viewingDistance tree $ reverse $ V.toList treesAbove
    treesVisibleBelow      = viewingDistance tree $ V.toList treesBelow
    score                  = getProduct $ foldMap
        (Product . max 1)
        [ treesVisibleToTheLeft
        , treesVisibleToTheRight
        , treesVisibleAbove
        , treesVisibleBelow
        ]

viewingDistance :: Tree -> [Tree] -> Int
viewingDistance (Tree _ s) xs = max 1 (length visible + hl)
  where
    (!visible, !same) = span ((< s) . treeSize) xs
    hl                = if null same then 0 else 1

newtype ScenicForest =
    ScenicForest { getScenicForest :: Vector (Vector ScenicTree) }

instance Show ScenicForest where
    show (ScenicForest forest) = showGrid forest

newtype Forest = Forest { getForest :: Vector (Vector Tree) }

instance Show Forest where
    show (Forest forest) = showGrid forest

showGrid :: Show a => Vector (Vector a) -> String
showGrid = intercalate "\n" . V.toList . fmap (mconcat . fmap show . V.toList)

mapForest :: (Vector (Vector Tree) -> Vector (Vector Tree)) -> Forest -> Forest
mapForest f = Forest . f . getForest

data ScenicTree = ScenicTree Int Tree

scenicScore :: ScenicTree -> Int
scenicScore (ScenicTree x _) = x

instance Show ScenicTree where
    show (ScenicTree score _) = "(" <> show score <> ")"

data Tree = Tree Visibility Int

treeSize :: Tree -> Int
treeSize (Tree _ s) = s

instance Show Tree where
    show (Tree Hidden  s) = "\x1b[37;2m" <> show s <> "\x1b[0m"
    show (Tree Visible s) = "\x1b[37;1m" <> show s <> "\x1b[0m"

isVisible :: Tree -> Bool
isVisible (Tree Hidden _) = False
isVisible _               = True

isVisiblePast :: Tree -> Tree -> Bool
isVisiblePast (Tree _ x) (Tree _ y) = x > y

setVisibility :: Visibility -> Tree -> Tree
setVisibility v = mapVisibility (<> v)

mapVisibility :: (Visibility -> Visibility) -> Tree -> Tree
mapVisibility f (Tree v s) = Tree (f v) s

safeSplitAt :: Int -> Vector a -> (Vector a, Vector a)
safeSplitAt n v
    | n == 0        = (mempty, v)
    | n == length v = (v, mempty)
    | otherwise     = V.splitAt n v

safeTail :: Vector a -> Vector a
safeTail v
    | V.null v  = v
    | otherwise = V.tail v

data Visibility = Hidden | Visible

instance Show Visibility where
    show Visible = "#"
    show Hidden  = "_"

instance Semigroup Visibility where
    Hidden  <> v = v
    Visible <> _ = Visible

data Pos = Pos
    { y :: Int
    , x :: Int
    }
    deriving (Eq, Ord)

instance Show Pos where
    show (Pos y x) = show (y, x)
