module Agent
(
    act
)
where

import Data

act :: GameState -> Action
-- dummy action
act = Clean . Just . head . hand
