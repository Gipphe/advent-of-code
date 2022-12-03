module Y2022
    ( y2022
    ) where

import Y2022.Day1
import Y2022.Day2
import Y2022.Day3
import Year

y2022 :: IO ()
y2022 = runYear 2022 [someDay1, someDay2, someDay3]
