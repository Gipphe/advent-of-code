module Args
    ( parseArgs
    , Opts(..)
    , Selection(..)
    , evalSelection
    , Specifier(..)
    ) where

import Data.Char (isDigit)
import Data.Foldable (foldl')
import Data.List (isInfixOf)
import Data.Set (Set)
import qualified Data.Set as S
import Util (split)

parseArgs :: [String] -> Either String Opts
parseArgs = go defaultOpts . normalizeFlags
  where
    go opts xs
        | null xs = Right opts
        | otherwise = case parseArg xs of
            (rest, Right opt) -> go (mergeOpts opts opt) rest
            (_   , Left err ) -> Left err

parseArg :: [String] -> ([String], Either String Opts)
parseArg = \case
    "--days" : days : rest -> (rest, Opts AllItems <$> readSelection days)
    "--years" : years : rest ->
        (rest, Opts <$> readSelection years <*> pure AllItems)
    unknown -> (unknown, Left $ "unknown flag: " <> show unknown)

normalizeFlags :: [String] -> [String]
normalizeFlags = fmap $ \case
    "-d"     -> "--days"
    "--day"  -> "--days"
    "-y"     -> "--years"
    "--year" -> "--years"
    x        -> x

data Opts = Opts
    { yearSelection :: Selection
    , daySelection  :: Selection
    }
    deriving Show

mergeOpts :: Opts -> Opts -> Opts
mergeOpts (Opts y1 d1) (Opts y2 d2) =
    Opts (pickSelection y1 y2) (pickSelection d1 d2)

pickSelection :: Selection -> Selection -> Selection
pickSelection AllItems      y             = y
pickSelection x             AllItems      = x
pickSelection (Specifier x) (Specifier y) = Specifier $ mergeSpecifiers x y

mergeSpecifiers :: Specifier -> Specifier -> Specifier
mergeSpecifiers (Single x Nothing) y = Single x (Just y)
mergeSpecifiers (Single x (Just rest)) y =
    Single x (Just $ mergeSpecifiers rest y)
mergeSpecifiers (Range x y Nothing) z = Range x y (Just z)
mergeSpecifiers (Range x y (Just rest)) z =
    Range x y (Just $ mergeSpecifiers rest z)

defaultOpts :: Opts
defaultOpts = Opts AllItems AllItems

data Selection
    = AllItems
    | Specifier Specifier
    deriving (Eq, Show)

data Specifier
    = Single Int (Maybe Specifier)
    | Range Int Int (Maybe Specifier)
    deriving (Eq, Show)

evalSelection :: Selection -> Set Int
evalSelection AllItems         = mempty
evalSelection (Specifier spec) = evalSpecifier spec

evalSpecifier :: Specifier -> Set Int
evalSpecifier (Single x rest) = S.insert x (maybe mempty evalSpecifier rest)
evalSpecifier (Range x y rest) =
    S.fromList [x .. y] <> maybe mempty evalSpecifier rest

readSelection :: String -> Either String Selection
readSelection xs = case traverse readSpecifier (split ',' xs) of
    Right [] -> Right AllItems
    Right specs ->
        maybe (Left "malformed specifier") (Right . Specifier) $ foldl'
            (\rest -> \case
                Single x _  -> Just $ Single x rest
                Range x y _ -> Just $ Range x y rest
            )
            Nothing
            specs
    Left x -> Left x

readSpecifier :: String -> Either String Specifier
readSpecifier xs
    | all isDigit xs = Right $ Single (read xs) Nothing
    | "-" `isInfixOf` xs = case split '-' xs of
        (x : y : _) -> Right $ Range (read x) (read y) Nothing
        x           -> Left $ "range does not have two items: " <> show x
    | otherwise = Left $ "invalid specifier: " <> xs
