module Location.Core where

import Data.IORef as IORefs
import Geodetics.Altitude as Alt
import Geodetics.Geodetic as Geo
-- import Numeric.Units.Dimensional.Prelude (meter, (*~))
import Numeric.Units.Dimensional.Prelude
import qualified Prelude as P
import System.IO.Unsafe (unsafePerformIO)

-- | Degrees, minutes and seconds into radians.  Copied from the
-- | tests for the geodetics package.
dms :: Int -> Int -> Double -> Dimensionless Double
dms d m s = fromIntegral d *~ degree + fromIntegral m *~ arcminute + s *~ arcsecond

-- TODO: figure out how to do this while still retaining safety.
currentPosition :: IORefs.IORef (Geo.Geodetic Geo.WGS84)
currentPosition = unsafePerformIO (IORefs.newIORef (Geo.Geodetic (dms 37 48 15.7068) (dms 22 16 15.9996) (9 *~ metre) Geo.WGS84))

parseCoords :: String -> Maybe (Geo.Geodetic Geo.WGS84)
parseCoords = Geo.readGroundPosition Geo.WGS84 


-- 2018-11-05 - punting on this.
-- maybeSetAltitude defaults to assuming altitudes are in metres.
-- maybeSetAltitude :: Maybe Double -> Maybe (Geo.Geodetic Geo.WGS84) -> Maybe (Geo.Geodetic Geo.WGS84)
-- maybeSetAltitude Nothing pos = pos
-- maybeSetAltitude _ Nothing = Nothing
-- maybeSetAltitude (Just alt) (Just pos) = Nothing -- Alt.setAltitude (alt *~ meter) (Just pos)

data Coordinates = Coordinates {
      latitude :: Double
    , longitude :: Double
    , altitude :: Double
}

newPosition :: Double -> Double -> Double -> Geo.Geodetic Geo.WGS84
newPosition lat lon alt = Geo.Geodetic (lat *~ degree) (lon *~ degree) (alt *~ metre) Geo.WGS84

-- TODO: do this without unsafePerformIO
getCurrentPosition :: Geo.Geodetic Geo.WGS84
getCurrentPosition = unsafePerformIO $ IORefs.readIORef currentPosition

-- TODO: do this without unsafePerformIO
setCurrentPosition :: Double -> Double -> Double -> Geo.Geodetic Geo.WGS84
setCurrentPosition lat lon alt =  unsafePerformIO $ do 
  IORefs.writeIORef currentPosition (newPosition lat lon alt)
  IORefs.readIORef currentPosition
