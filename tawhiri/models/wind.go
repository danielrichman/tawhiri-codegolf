package models

import (
    "math"

    "../../tawhiri"
    "../dataset"
)

type Wind struct {
    DS dataset.Dataset
}

const EarthRadius = 6371009.0

func (w Wind) Eval(s tawhiri.State) (d tawhiri.Delta) {
    wind_u, wind_v := w.DS.Interpolate(s.Now, s.Lat, s.Lon, s.Alt)
    r := EarthRadius + s.Alt
    d.DLat = 180 / math.Pi * wind_v / r
    d.DLon = 180 / math.Pi * wind_u / (r * math.Cos(s.Lat*math.Pi/180))
    return
}
