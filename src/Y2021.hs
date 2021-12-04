module Y2021
    ( y2021
    ) where

import Y2021.Day1
import Y2021.Day2
import Y2021.Day3
import Year

y2021 :: IO ()
y2021 = runYear 2021 [someDay1, someDay2, someDay3]
