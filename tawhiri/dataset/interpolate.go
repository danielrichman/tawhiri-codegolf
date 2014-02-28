package dataset

import "time"

func (ds *Dataset) Interpolate(abstime time.Time, lat, lon, alt float64) (float64, float64) {
    hour := float64(abstime.Sub(ds.When)) / float64(time.Hour)

    lerps := pick3(hour, lat, lon)

    altidx := search(ds, lerps, alt)
    lower := interp3(ds, lerps, height, altidx)
    upper := interp3(ds, lerps, height, altidx+1)

    lerp := 0.5
    if lower != upper {
        lerp = (upper - alt) / (upper - lower)
    }

    alt_lerp := lerp1{altidx, lerp}
    u := interp4(ds, lerps, alt_lerp, windu)
    v := interp4(ds, lerps, alt_lerp, windv)
    return u, v
}

type lerp1 struct {
    index int
    lerp  float64
}

type lerp3 struct {
    hour, lat, lon int
    lerp           float64
}

func pick(left float64, step float64, n int, value float64) [2]lerp1 {
    a := (value - left) / step
    b := int(a)
    if b < 0 || b >= n-1 {
        panic("value out of range")
    }
    l := a - float64(b)
    return [2]lerp1{{b, 1 - l}, {b + 1, l}}
}

func pick3(hour, lat, lon float64) (lerps [8]lerp3) {
    lhour := pick(0, 3, 65, hour)
    llat := pick(-90, 0.5, 361, lat)
    llon := pick(0, 0.5, 720+1, lon)
    if llon[1].index == 720 {
        llon[1].index = 0
    }

    i := 0
    for _, a := range lhour {
        for _, b := range llat {
            for _, c := range llon {
                p := a.lerp * b.lerp * c.lerp
                lerps[i] = lerp3{a.index, b.index, c.index, p}
                i += 1
            }
        }
    }

    return
}

func interp3(ds *Dataset, lerps [8]lerp3, variable, level int) float64 {
    r := 0.0
    for _, lerp := range lerps {
        v := ds.Array[lerp.hour][level][variable][lerp.lat][lerp.lon]
        r += v * lerp.lerp
    }
    return r
}

func search(ds *Dataset, lerps [8]lerp3, target float64) int {
    lower, upper := 0, 45
    for lower < upper {
        mid := (lower + upper + 1) / 2
        test := interp3(ds, lerps, height, mid)
        if target <= test {
            upper = mid - 1
        } else {
            lower = mid
        }
    }
    return lower
}

func interp4(ds *Dataset, lerps [8]lerp3, alt_lerp lerp1, variable int) float64 {
    lower := interp3(ds, lerps, variable, alt_lerp.index)
    // and we can infer what the other lerp1 is:
    upper := interp3(ds, lerps, variable, alt_lerp.index+1)
    return lower*alt_lerp.lerp + upper*(1-alt_lerp.lerp)
}
