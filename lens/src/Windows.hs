{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Windows where

import Control.Lens (Field1 (_1), Field2 (_2), Getting, Ixed (ix), Lens', Prism', Traversal', ignored, index, over, toListOf, traversed, (^.), (^..))
import Control.Lens.Unsound (adjoin)
import Data.Foldable (Foldable (toList))
import Types

allWindowsInProject :: [Window]
allWindowsInProject = project ^. allWindows

allGlazingsInProject :: [Glazing]
allGlazingsInProject = allWindowsInProject ^. traverse . windowToGlazing 0

allWindows :: Traversal' Project [Window]
allWindows = listWindow `adjoin` listWindowOptional

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

windowToGlazing :: Int -> Traversal' Window [Glazing]
windowToGlazing indexFrame =
  listField
    . traversed
    . index indexFrame
    . fieldToFilling
    . _Glass

fieldToFilling :: Traversal' Field Filling
fieldToFilling f = \case
  Standard slots -> Standard <$> slotsToFillings f slots
  HST filling -> HST <$> f filling
  field -> pure field

slotsToFillings :: Traversal' [Slot] Filling
slotsToFillings = traverse . listFilling . traverse
