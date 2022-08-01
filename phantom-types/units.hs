{-# LANGUAGE GeneralizedNewtypeDeriving #-}

newtype Distance a = Distance Double
  deriving (Num, Show)

data Kilometer
data Mile

marathonDistance :: Distance Kilometer
marathonDistance = Distance 42.195

distanceKmToMiles :: Distance Kilometer -> Distance Mile
distanceKmToMiles (Distance km) = Distance (0.621371 * km)

marathonDistanceInMiles :: Distance Mile
marathonDistanceInMiles = distanceKmToMiles marathonDistance

-- doesn't compile: Couldn't match type ‘Mile’ with ‘Kilometer’
-- marathonDistanceInMilesError :: Distance Mile
-- marathonDistanceInMilesError = distanceKmToMiles $ distanceKmToMiles marathonDistance