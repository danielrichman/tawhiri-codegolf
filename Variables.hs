module Variables
( Altitude
, Position(..)
, Delta
, Delta3D(..)
, Delta2D(..)
, Time(..)
, InitialConditions(..)
, (.+)
, (.*)
, wrapLongitude
, replaceAltitude
, fromDelta2D
, timeAfterICs
, (..+)
) where

import Data.Monoid
import Data.Fixed
import Data.Time.Clock
import Data.Time.Format()

type Altitude = Double
data Position = Position Double Double Altitude deriving (Show, Read, Eq)
data Delta3D = Delta3D Double Double Altitude deriving (Show, Read, Eq)
data Delta2D = Delta2D Double Double deriving (Show, Read, Eq)

infixl 6 .+
infixl 7 .*

class (Monoid a) => Delta a where
    (.+) :: Position -> a -> Position
    (.*) :: a -> Double -> a

instance Monoid Delta3D where
    mempty = Delta3D 0 0 0
    d1 `mappend` d2 = Delta3D (lat1 + lat2) (lon1 + lon2) (alt1 + alt2)
        where Delta3D lat1 lon1 alt1 = d1
              Delta3D lat2 lon2 alt2 = d2

instance Delta Delta3D where
    posn .+ delta = Position (lat + dlat) (lon + dlon) (alt + dalt)
        where Position lat lon alt = posn
              Delta3D dlat dlon dalt = delta
    Delta3D dlat dlon dalt .* s = Delta3D (dlat * s) (dlon * s) (dalt * s)

instance Monoid Delta2D where
    mempty = Delta2D 0 0
    d1 `mappend` d2 = Delta2D (lat1 + lat2) (lon1 + lon2)
        where Delta2D lat1 lon1 = d1
              Delta2D lat2 lon2 = d2

instance Delta Delta2D where
    posn .+ delta = Position (lat + dlat) (lon + dlon) alt
        where Position lat lon alt = posn
              Delta2D dlat dlon = delta
    Delta2D dlat dlon .* s = Delta2D (dlat * s) (dlon * s)

wrapLongitude :: Position -> Position
wrapLongitude (Position lat lon alt) = Position lat lon' alt
    where lon' = let t = lon `mod'` 360 in
                 if t < 0 then t + 360 else t

replaceAltitude :: Position -> Altitude -> Position
replaceAltitude (Position lat lon _) alt =
        Position lat lon alt

fromDelta2D :: Delta2D -> Delta3D
fromDelta2D (Delta2D lat lon) = Delta3D lat lon 0

data Time = Time { now :: UTCTime, flightTime :: Double, itemTime :: Double }
    deriving (Show, Read, Eq)

data InitialConditions =
    InitialConditions { position :: Position, datetime :: UTCTime,
                        ic_flightTime :: Double }

timeAfterICs :: InitialConditions -> Double -> Time
timeAfterICs initial_conditions item_time =
        Time now' (ic_flightTime initial_conditions + item_time) item_time
    where ic_dt = datetime initial_conditions
          now' = addUTCTime (realToFrac item_time) ic_dt

infixl 6 ..+

(..+) :: Time -> Double -> Time
Time now' flightTime' itemTime' ..+ delta =
        Time now'' (flightTime' + delta) (itemTime' + delta)
    where now'' = addUTCTime (realToFrac delta) now'
