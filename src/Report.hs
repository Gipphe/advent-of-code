module Report where

import Data.Time (NominalDiffTime)

data Report = Report
    { reportYear :: Integer
    , reportDays :: [ReportDay]
    }

data ReportDay = ReportDay
    { reportDay      :: Integer
    , reportDayTasks :: [ReportTask]
    }

data ReportTask = ReportTask
    { reportTask       :: Integer
    , reportTaskResult :: Int
    , reportTaskTime   :: NominalDiffTime
    }
