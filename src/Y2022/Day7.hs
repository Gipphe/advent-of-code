module Y2022.Day7
    ( someDay7
    , computeTask1
    , computeTask2
    , input
    ) where

import Data.Foldable (foldl')
import Data.Functor.Identity (Identity(..))
import Data.List (isPrefixOf, sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Sequence (Seq(..), (|>))
import Day (Day, SomeDay(..), Task, runTask)
import Util (split)
import Y2022.Input (day7Input)

input :: Dir
input =
    buildFileTree
        . parseInput
        . tail
        . filter (not . ("dir " `isPrefixOf`))
        . filter (not . ("$ ls" `isPrefixOf`))
        . lines
        $ day7Input

someDay7 :: SomeDay
someDay7 = SomeDay day7

day7 :: Day 7 ()
day7 = do
    runTask day7Task1
    runTask day7Task2

day7Task1 :: Task 1 Int
day7Task1 = pure $ computeTask1 input

day7Task2 :: Task 2 Int
day7Task2 = pure $ computeTask2 input

computeTask1 :: Dir -> Int
computeTask1 =
    sum . fmap dirSize . findWithSizeOn (< 100000) . snd . calculateDirSizes

computeTask2 :: Dir -> Int
computeTask2 x = head . sort . fmap dirSize $ findWithSizeOn
    (> minimumSpaceToFreeUp)
    dirs
  where
    (dirs@(Dir _ size _)) = snd $ calculateDirSizes x
    remainingSpace        = capacity - size
    minimumSpaceToFreeUp  = requiredFreeSpace - remainingSpace

parseInput :: [String] -> [FileWithPath]
parseInput = snd . foldl' go (Empty, [])
  where
    go (path, coll) line
        | line == "$ cd .."   = (cdUp path, coll)
        | cdPrefix == "$ cd " = (cdDown path dirName, coll)
        | otherwise           = (path, parseFileWithPath path line : coll)
        where (cdPrefix, dirName) = splitAt 5 line

type Path = Seq String

cdUp :: Path -> Path
cdUp Empty      = Empty
cdUp (xs :|> _) = xs

cdDown :: Path -> String -> Path
cdDown path folder = path |> folder

data FileWithPath = FileWithPath Path String Int
    deriving Show

parseFileWithPath :: Path -> String -> FileWithPath
parseFileWithPath path x = case split ' ' x of
    (fileSize : fileName : _) -> FileWithPath path fileName (read fileSize)
    unknown                   -> error $ "invalid file: " <> show unknown

buildFileTree :: [FileWithPath] -> Dir
buildFileTree = foldr addToDir (Dir "/" 0 M.empty)

findWithSizeOn :: (Int -> Bool) -> Dir -> [Dir]
findWithSizeOn predicate (Dir dirName dirSize items)
    | predicate dirSize = Dir dirName dirSize items : subDirsThatSatisfyPred
    | otherwise         = subDirsThatSatisfyPred
  where
    subDirs                = M.elems $ getSubDirs items
    subDirsThatSatisfyPred = mconcat (fmap (findWithSizeOn predicate) subDirs)

addToDir :: FileWithPath -> Dir -> Dir
addToDir (FileWithPath path fileName fileSize) (Dir dirName dirSize items) =
    case path of
        Empty -> Dir dirName dirSize (M.insert fileName (ItemFile file) items)
        (subDirName :<| rest) ->
            let
                subDir = M.findWithDefault
                    (Dir subDirName 0 M.empty)
                    subDirName
                    (getSubDirs items)
            in
                Dir
                    dirName
                    dirSize
                    (M.insert
                        subDirName
                        (ItemDir $ addToDir
                            (FileWithPath rest fileName fileSize)
                            subDir
                        )
                        items
                    )
    where file = File fileName fileSize

calculateDirSizes :: Dir -> (Int, Dir)
calculateDirSizes (Dir dirName _ items) =
    ( totalSubdirSize + totalFileSizes
    , Dir dirName totalDirSize (M.union (fmap ItemDir subDirs) items)
    )
  where
    totalDirSize    = totalSubdirSize + totalFileSizes
    totalSubdirSize = sum . fmap fst . M.elems $ subDirsWithSizes
    subDirs         = fmap snd subDirsWithSizes
    subDirsWithSizes :: Map String (Int, Dir)
    subDirsWithSizes = calculateDirSizes <$> getSubDirs items
    totalFileSizes :: Int
    totalFileSizes = sum . fmap fileSize . M.elems $ getDirFiles items

getSubDirs :: Map String Item -> Map String Dir
getSubDirs = runIdentity . M.traverseMaybeWithKey
    (\_ -> Identity . \case
        ItemFile _   -> Nothing
        ItemDir  dir -> Just dir
    )

getDirFiles :: Map String Item -> Map String File
getDirFiles = runIdentity . M.traverseMaybeWithKey
    (\_ -> Identity . \case
        ItemFile f -> Just f
        ItemDir  _ -> Nothing
    )

data Dir = Dir
    { dirName  :: String
    , dirSize  :: Int
    , dirItems :: Map String Item
    }
    deriving Show

data File = File
    { fileName :: String
    , fileSize :: Int
    }
    deriving Show

data Item
    = ItemDir Dir
    | ItemFile File
    deriving (Show)


capacity :: Int
capacity = 70000000

requiredFreeSpace :: Int
requiredFreeSpace = 30000000
