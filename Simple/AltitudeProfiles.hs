module Simple.AltitudeProfiles ( linear, constant ) where

import Variables
import Models

linear :: Altitude -> Double -> AltitudeProfile
linear initial_altitude speed time = initial_altitude + (itemTime time) * speed

constant :: Altitude -> AltitudeProfile
constant value _ = value
