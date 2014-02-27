module Simple.Wind ( wind ) where

import Variables
import Models
import Dataset

wind :: Dataset -> Model2D
wind dataset x@(Position lat _ alt) t =
    let (wind_u, wind_v) = interpolate dataset x t
        r = 6371009 + alt
        dlat = 180 / pi * wind_v / r
        dlon = 180 / pi * wind_u / (r * cos (lat * pi / 180))
    in
        Delta2D dlat dlon
