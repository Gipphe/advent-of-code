module Year
    ( runYear
    ) where

import Data.List (intersperse)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Time.Clock.POSIX (getPOSIXTime)

import Args (Selection, evalSelection)
import Day (SomeDay, runSomeDay, someDayNumber)

runYear :: Int -> [SomeDay] -> Selection -> IO ()
runYear year days daySelection = do
    putStrLn $ "## Year " <> show year
    startTime <- getPOSIXTime
    sequence_ $ intersperse (putStrLn "\n") $ runSomeDay <$> selectedDays
    endTime <- getPOSIXTime
    putStrLn
        $  "\n\nTook "
        <> show (endTime - startTime)
        <> " to run year "
        <> show year
  where
    selection    = evalSelection daySelection
    selectedDays = if S.size selection == 0
        then days
        else M.elems $ M.restrictKeys
            (M.fromList $ fmap (\d -> (someDayNumber d, d)) days)
            (evalSelection daySelection)
