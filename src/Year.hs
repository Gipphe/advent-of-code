module Year
    ( runYear
    ) where

import Data.List (intersperse)
import Data.Time.Clock.POSIX (getPOSIXTime)

import Day (SomeDay, runSomeDay)

runYear :: Int -> [SomeDay] -> IO ()
runYear year days = do
    putStrLn $ "## Year " <> show year
    startTime <- getPOSIXTime
    sequence_ $ intersperse (putStrLn "\n") $ runSomeDay <$> days
    endTime <- getPOSIXTime
    putStrLn
        $  "\n\nTook "
        <> show (endTime - startTime)
        <> " to run year "
        <> show year
