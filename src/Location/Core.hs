module Location.Core where

import Geodetics.Altitude as Alt
import Geodetics.Geodetic as Geo
import Numeric.Units.Dimensional.Prelude (meter, (*~))

parseCoords :: String -> Maybe (Geo.Geodetic Geo.WGS84)
parseCoords = Geo.readGroundPosition Geo.WGS84 

-- maybeSetAltitude defaults to assuming altitudes are in metres.
maybeSetAltitude :: Maybe Double -> Maybe (Geo.Geodetic Geo.WGS84) -> Maybe (Geo.Geodetic Geo.WGS84)
maybeSetAltitude Nothing pos = pos
maybeSetAltitude _ Nothing = Nothing
maybeSetAltitude (Just alt) (Just pos) = Nothing -- Alt.setAltitude (alt *~ meter) (Just pos)
