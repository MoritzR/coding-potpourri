module Lib where

import qualified Data.Set as Set
import Control.Lens.Operators ((.~), (%~))
import Control.Lens.Combinators (contains)
import Data.Function ((&))

setContaining3 = Set.fromList [1,2,4] & contains 3 .~ True

setWithout4 = Set.fromList [1,2,4] & contains 4 %~ not

{-
> Set.fromList [1,2,4]
Set (1,2,4)

> Set.fromList [1,2,4] & contains 3 .~ True
Set (1,2,3,4)
-}
