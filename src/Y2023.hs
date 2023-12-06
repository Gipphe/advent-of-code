module Y2023 (
  y2023,
)
where

import Args (Selection)
import Y2023.Day1 (someDay1)
import Y2023.Day2 (someDay2)
import Year (runYear)

y2023 :: Selection -> IO ()
y2023 =
  runYear
    2023
    [ someDay1
    , someDay2
    ]
