{-# LANGUAGE TemplateHaskell #-}

module Types where

import Control.Lens (makeLenses)

data Project = Project
  { _listWindow :: [Window],
    _listWindowOptional :: [Window]
  }

data Window = Window
  { _listField :: [Field]
  }

data Field
  = Standard
      { _listSlot :: [Slot]
      }
  | HST
      { _filling :: Filling
      }
  | Custom
      { picturePath :: String
      }
  | Wall

data Slot = Slot
  { _listFilling :: [Filling]
  }

data Filling
  = Glass
      { _listGlazing :: [Glazing]
      }
  | PVC

data Glazing = Glazing

makeLenses ''Project
makeLenses ''Window
makeLenses ''Field
makeLenses ''Slot
makeLenses ''Filling
makeLenses ''Glazing