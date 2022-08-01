type Distance<Unit> = number & Unit

type Kilometer = { readonly _tag: unique symbol }
type Mile = { readonly _tag: unique symbol }


const marathonDistance: Distance<Kilometer> = 42.195 as Distance<Kilometer>

const distanceKmToMiles: (km: Distance<Kilometer>) => Distance<Mile> = km => 0.621371 * km as Distance<Mile>

const marathonDistanceInMiles: Distance<Mile> = distanceKmToMiles(marathonDistance)

// Argument of type 'Distance<Mile>' is not assignable to parameter of type 'Distance<Kilometer>'
// @ts-expect-error
const marathonDistanceInMilesError: Distance<Mile> = distanceKmToMiles(distanceKmToMiles(marathonDistance))