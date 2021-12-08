module Y2021.Day8 where

import Data.Foldable (foldl')
import Data.Functor (($>))
import Data.List (find)
import qualified Data.Map.Lazy as M
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Void (Void)
import Text.Megaparsec
    (Parsec, choice, errorBundlePretty, parse, sepEndBy1, some)
import Text.Megaparsec.Char (char, space)

import Day (SomeDay(..), Task, runTask)
import Util (digitsToDecimal)
import Y2021.Input (day8Input)

someDay8 :: SomeDay
someDay8 = SomeDay @8 do
    runTask day8Task1
    runTask day8Task2

day8Task1 :: Task 1 Int
day8Task1 = pure $ computeTask1 input

day8Task2 :: Task 2 Int
day8Task2 = pure $ computeTask2 input

computeTask1 :: [Message] -> Int
computeTask1 = length . mconcat . fmap (filter isUniqueDigit . messageMessage)

computeTask2 :: [Message] -> Int
computeTask2 = sum . fmap (digitsToDecimal . deduceMessage)

deduceMessage :: Message -> [Int]
deduceMessage message =
    foldr ((:) . fromMaybe (error "not found") . flip M.lookup legend) []
        $ messageMessage message
  where
    (uniques, unknowns) = partitionUnique $ messageKey message
    (one    , _       ) = fromMaybe (error "no 1") $ find ((== 1) . snd) uniques
    (four   , _       ) = fromMaybe (error "no 4") $ find ((== 4) . snd) uniques
    interpreted         = deduceSeries one four <$> unknowns
    legend              = M.fromList $ uniques <> interpreted

partitionUnique :: [Series] -> ([(Series, Int)], [Series])
partitionUnique = foldl' go ([], [])
  where
    go (interpreted, unknowns) x
        | l == 2    = ((x, 1) : interpreted, unknowns)
        | l == 4    = ((x, 4) : interpreted, unknowns)
        | l == 3    = ((x, 7) : interpreted, unknowns)
        | l == 7    = ((x, 8) : interpreted, unknowns)
        | otherwise = (interpreted, x : unknowns)
        where l = length $ getSeries x


-- | Deduces the represented digit in the series.
-- n: 1, 4, t
-- 0: 2, 3, 6
-- 2: 1, 2, _
-- 3: 2, 3, 5
-- 5: 1, 3, 5
-- 6: 1, 3, 6
-- 9: 2, 4, _
deduceSeries :: Series -> Series -> Series -> (Series, Int)
deduceSeries (Series one) (Series four) s@(Series x) =
    case (diffOne, diffFour, len) of
        (2, 3, 6) -> (s, 0)
        (1, 2, _) -> (s, 2)
        (2, 3, 5) -> (s, 3)
        (1, 3, 5) -> (s, 5)
        (1, 3, 6) -> (s, 6)
        (2, 4, _) -> (s, 9)
        series    -> error $ "Not an unknown series: " <> show series
  where
    diffOne  = S.size $ x `S.intersection` one
    diffFour = S.size $ x `S.intersection` four
    len      = S.size x

isUniqueDigit :: Series -> Bool
isUniqueDigit x
    | l == 2 || l == 4 || l == 3 || l == 7 = True
    | otherwise                            = False
    where l = length $ getSeries x

input :: [Message]
input = parseInput day8Input

data Message = Message
    { messageKey     :: [Series]
    , messageMessage :: [Series]
    }
    deriving Show

data Seg
    = A | B | C | D | E | F | G
    deriving (Eq, Ord, Show)

parseSeg :: Char -> Either String Seg
parseSeg = \case
    'a' -> Right A
    'b' -> Right B
    'c' -> Right C
    'd' -> Right D
    'e' -> Right E
    'f' -> Right F
    'g' -> Right G
    x   -> Left $ "unrecognized seg: " <> show x

newtype Series = Series { getSeries :: Set Seg }
    deriving (Eq, Ord, Show)

parseInput :: String -> [Message]
parseInput = either (error . errorBundlePretty) id . parse messagesP ""

messagesP :: Parser [Message]
messagesP = sepEndBy1 messageP (char '\n')

messageP :: Parser Message
messageP = do
    messageKey <- someSegsP
    char '|'
    space
    messageMessage <- someSegsP
    pure $ Message { messageKey, messageMessage }

someSegsP :: Parser [Series]
someSegsP = sepEndBy1 segsP (char ' ')

segsP :: Parser Series
segsP = Series . S.fromList <$> some segP

segP :: Parser Seg
segP = choice
    [ char 'a' $> A
    , char 'b' $> B
    , char 'c' $> C
    , char 'd' $> D
    , char 'e' $> E
    , char 'f' $> F
    , char 'g' $> G
    ]

type Parser = Parsec Void String
