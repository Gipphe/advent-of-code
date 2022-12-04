module Run where

import Data.Time (NominalDiffTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
-- import Text.Layout.Table
--     (Default(..), numCol, rowsG, tableString, titlesH, unicodeS)

data Year' day = Year
    { yearNumber :: !Int
    , yearDays   :: ![day]
    }

type Year = Year' Day

data Day' task = Day
    { dayNumber :: !Int
    , dayTasks  :: ![task]
    }

type Day = Day' Task

data Task = Task
    { taskNumber :: !Int
    , taskResult :: !(IO Int)
    }

type YearTable = [(String, Int, NominalDiffTime)]

data Timed a = Timed
    { timedTime   :: !NominalDiffTime
    , timedEntity :: !a
    }

data Evaluated a = Evaluated
    { evaluatedResult :: {-# UNPACK #-} !Int
    , evaluatedEntity :: !a
    }

runYear :: Year -> IO (Timed (Year' (Timed (Day' (Timed (Evaluated Task))))))
runYear (Year year days) = do
    start   <- getPOSIXTime
    results <- traverse runDay days
    end     <- getPOSIXTime
    pure $ Timed (end - start) (Year year results)

runDay :: Day -> IO (Timed (Day' (Timed (Evaluated Task))))
runDay (Day day tasks) = do
    start   <- getPOSIXTime
    results <- traverse runTask tasks
    end     <- getPOSIXTime
    pure $ Timed (end - start) $ Day day results

runTask :: Task -> IO (Timed (Evaluated Task))
runTask t = do
    start  <- getPOSIXTime
    result <- taskResult t
    end    <- getPOSIXTime
    pure $ Timed (end - start) $ Evaluated result t

-- showYear :: Timed (Year' (Timed (Day' (Timed (Evaluated Task))))) -> String
-- showYear (Timed yearTime (Year yearNum days)) =
--     yearHeading <> "\n" <> tableString
--         [def, numCol, numCol]
--         unicodeS
--         (titlesH ["Task", "Result", "Time"])
--         (rowGroupDay <$> days)
--   where
--     yearHeading = "Year " <> show yearNum <> ": " <> show yearTime
--     rowGroupDay (Timed dayTime (Day dayNumber tasks)) =
--         rowsG
--             $ ["Day " <> show dayNumber, "", show dayTime]
--             : (rowTask dayNumber <$> tasks)
--     rowTask dayNumber (Timed taskTime (Evaluated result (Task taskNumber _))) =
--         [ "Day " <> show dayNumber <> " Task " <> show taskNumber
--         , show result
--         , show taskTime
--         ]
