module Location.Core where

import Data.IORef as IORefs
import Geodetics.Altitude as Alt
import Geodetics.Geodetic as Geo
import Numeric.Units.Dimensional.Prelude (meter, (*~))

currentPosition :: IORefs.IORef (Geo.Geodetic Geo.WGS84)
currentPosition = IORefs.newIORef

parseCoords :: String -> Maybe (Geo.Geodetic Geo.WGS84)
parseCoords = Geo.readGroundPosition Geo.WGS84 

-- 2018-11-05 - punting on this.
-- maybeSetAltitude defaults to assuming altitudes are in metres.
-- maybeSetAltitude :: Maybe Double -> Maybe (Geo.Geodetic Geo.WGS84) -> Maybe (Geo.Geodetic Geo.WGS84)
-- maybeSetAltitude Nothing pos = pos
-- maybeSetAltitude _ Nothing = Nothing
-- maybeSetAltitude (Just alt) (Just pos) = Nothing -- Alt.setAltitude (alt *~ meter) (Just pos)
