{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Lib
  ( someFunc,
  )
where

import Control.Lens (Field1 (_1), Field2 (_2), Getting, Ixed (ix), Lens', Prism', Traversal', ignored, index, over, toListOf, traversed, (^.))
import Control.Lens.Unsound (adjoin)
import Data.Foldable (Foldable (toList))
import Types

someFunc :: IO ()
someFunc = putStrLn "someFunc"

glassFilling :: Filling
glassFilling =
  Glass
    { _listGlazing = [Glazing]
    }

window1 :: Window
window1 =
  Window
    { _listField =
        [ Standard
            { _listSlot =
                [ Slot
                    { _listFilling = [glassFilling]
                    }
                ]
            }
        ]
    }

project :: Project
project =
  Project
    { _listWindow = [window1],
      _listWindowOptional = [window1]
    }

glazings :: Int -> Traversal' Window Field
glazings indexFrame = listField . traversed . index indexFrame

slotsToFillings :: Traversal' [Slot] Filling
slotsToFillings = traverse . listFilling . traverse

fieldToFillings :: Traversal' Field Filling
fieldToFillings f = \case
  Standard slots -> Standard <$> slotsToFillings f slots
  HST filling -> HST <$> f filling
  field -> pure field

allWindows :: Traversal' Project [Window]
allWindows = listWindow `adjoin` listWindowOptional

allWindowsInProject :: [Window]
allWindowsInProject = project ^. allWindows
