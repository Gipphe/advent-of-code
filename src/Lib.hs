module Lib
    ( main
    ) where

import Data.List (intersperse)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Args (Selection, daySelection, evalSelection, parseArgs, yearSelection)
import Data.Foldable (sequenceA_)
import System.Environment (getArgs)
import Y2020
import Y2021
import Y2022
import Y2023

main :: IO ()
main = do
    args <- parseArgs <$> getArgs
    case args of
        Left err -> error err
        Right opts ->
            let
                selection     = evalSelection (yearSelection opts)
                selectedYears = if S.size selection == 0
                    then years
                    else M.restrictKeys
                        years
                        (evalSelection (yearSelection opts))
            in
                sequenceA_
                $ intersperse (putStrLn "\n---------------\n")
                . fmap ($ daySelection opts)
                . M.elems
                $ selectedYears

years :: Map Int (Selection -> IO ())
years = M.fromList [(2020, y2020), (2021, y2021), (2022, y2022), (2023, y2023)]
