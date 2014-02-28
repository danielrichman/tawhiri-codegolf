package main

import (
    "fmt"
    "math"
    "os"
    "path"
    "time"
    "unsafe"
)

// Variables
type Position struct {
    Lat, Lon, Alt float64
}

type Delta struct {
    DLat, DLon, DAlt float64
}

func WrapLongitude(lon float64) float64 {
    for lon < 0 {
        lon += 360
    }
    for lon >= 360 {
        lon -= 360
    }
    return lon
}

type Time struct {
    Now        time.Time
    FlightTime float64
    ItemTime   float64
}

type State struct {
    Time
    Position
}

// Models
type Model interface {
    Eval(State) Delta
}

type TerminationCondition interface {
    Eval(State) bool
}

type LinearCombination struct {
    Models []Model
}

func (c LinearCombination) Eval(state State) (d Delta) {
    for _, m := range c.Models {
        e := m.Eval(state)
        d.DLat += e.DLat
        d.DLon += e.DLon
        d.DAlt += e.DAlt
    }
    return
}

// Solvers
type Solver interface {
    Run(Model, TerminationCondition, State, chan State)
}

type Configuration struct {
    Model                Model
    TerminationCondition TerminationCondition
    Solver               Solver
}

func (c Configuration) Run(ics State, out chan State) {
    c.Solver.Run(c.Model, c.TerminationCondition, ics, out)
}

type Chain []Configuration

func (cs Chain) Run(ics State, out chan State) {
    out <- ics

    for _, c := range cs {
        pipe := make(chan State, 100)
        go c.Run(ics, pipe)

        // drop the first point (don't repeat it)
        <-pipe

        var point State
        for point = range pipe {
            out <- point
        }

        ics = point
        ics.ItemTime = 0
    }

    close(out)
}

func Decimate(n int, in, out chan State) {
    i := 0
    var point State
    for point = range in {
        if i%n == 0 {
            out <- point
        }
        i += 1
    }
    // always emit the last point
    if i%n != 1 {
        out <- point
    }
    close(out)
}

// Convenience functions
func InitialConditions(t time.Time, lat, lon, alt float64) State {
    return State{Time{t, 0, 0}, Position{lat, lon, alt}}
}

func MakeLinearCombination(models ...Model) LinearCombination {
    return LinearCombination{models}
}

type array [65][47][3][361][720]float64

const ds_size = int64(unsafe.Sizeof(array{}))

const (
    height = 0
    windu  = 1
    windv  = 2
)

type Dataset struct {
    Array *array
    When  time.Time
}

func init() {
    if ds_size != 19057334400 {
        panic("dataset array type has incorrect size")
    }
}

func filename(when time.Time, directory string) string {
    return path.Join(directory, when.Format("2006010215"))
}

func Open(when time.Time, directory string) Dataset {
    // if when != when.Truncate(time.Hour * 3) || when.Location() != time.UTC {
    //    panic("invalid dataset time")
    // }

    file, err := os.Open(filename(when, directory))
    if err != nil {
        panic(err)
    }

    data, size, err := mmap(file)
    if err != nil {
        panic(err)
    }
    if size != ds_size {
        panic("dataset incorrect size")
    }

    return Dataset{(*array)(data), when}
}

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

type Wind struct {
    DS Dataset
}

const EarthRadius = 6371009.0

func (w Wind) Eval(s State) (d Delta) {
    wind_u, wind_v := w.DS.Interpolate(s.Now, s.Lat, s.Lon, s.Alt)
    r := EarthRadius + s.Alt
    d.DLat = 180 / math.Pi * wind_v / r
    d.DLon = 180 / math.Pi * wind_u / (r * math.Cos(s.Lat*math.Pi/180))
    return
}

type ConstantAltitude struct {
}

func (c ConstantAltitude) Eval(_ State) (out Delta) {
    return
}

type Linear struct {
    Speed float64
}

func (l Linear) Eval(_ State) (out Delta) {
    out.DAlt = l.Speed
    return
}

type BurstAltitude struct {
    Alt float64
}

func (b BurstAltitude) Eval(s State) bool {
    return s.Alt >= b.Alt
}

type LandedMSL struct {
}

func (_ LandedMSL) Eval(s State) bool {
    return s.Alt <= 0
}

type Timeout struct {
    Seconds float64
}

func (to Timeout) Eval(s State) bool {
    return s.ItemTime >= to.Seconds
}

type ForwardsEuler struct {
    Step float64
}

func (fe ForwardsEuler) Run(model Model, tc TerminationCondition, ics State, out chan State) {
    state := ics
    out <- state

    for !tc.Eval(state) {
        x_dot := model.Eval(state)
        state.Lat += x_dot.DLat * fe.Step
        state.Lon += x_dot.DLon * fe.Step
        state.Alt += x_dot.DAlt * fe.Step
        state.Now = state.Now.Add(time.Duration(fe.Step) * time.Second)
        state.FlightTime += fe.Step
        state.ItemTime += fe.Step
        state.Lon = WrapLongitude(state.Lon)
        out <- state
    }

    close(out)
}

func main() {
    start := time.Now()

    ds_time := time.Date(2014, 02, 18, 12, 0, 0, 0, time.UTC)
    ds := Open(ds_time, "/opt/wind")

    model := MakeLinearCombination(Wind{ds}, Linear{5})
    termcond := BurstAltitude{30000}
    solver := ForwardsEuler{1}
    config := Configuration{model, termcond, solver}
    chain := Chain{config}
    launch_time := time.Date(2014, 02, 19, 15, 0, 0, 0, time.UTC)
    ics := InitialConditions(launch_time, 52.2135, 0.0964, 0)

    fmt.Println("Setup took", time.Now().Sub(start))

    loopstart := time.Now()

    for i := 0; i < 100; i++ {
        pipe := make(chan State, 100)
        out := make(chan State, 100)

        go chain.Run(ics, pipe)
        go Decimate(60, pipe, out)

        for _ = range out {
        }
    }

    took := time.Now().Sub(loopstart)

    fmt.Println("100 iterations", took, "total", took/100, "avg")
}
