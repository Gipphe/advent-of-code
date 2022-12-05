module Y2022
    ( y2022
    ) where

import Args (Selection)
import Y2022.Day1
import Y2022.Day2
import Y2022.Day3
import Y2022.Day4
import Y2022.Day5
import Year

y2022 :: Selection -> IO ()
y2022 = runYear 2022 [someDay1, someDay2, someDay3, someDay4, someDay5]
