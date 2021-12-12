module Y2021
    ( y2021
    ) where

import Y2021.Day1
import Y2021.Day2
import Y2021.Day3
import Y2021.Day4
import Y2021.Day5
import Y2021.Day6
import Y2021.Day7
import Y2021.Day8
import Y2021.Day9
import Year

y2021 :: IO ()
y2021 = runYear
    2021
    [ someDay1
    , someDay2
    , someDay3
    , someDay4
    , someDay5
    , someDay6
    , someDay7
    , someDay8
    , someDay9
    ]
