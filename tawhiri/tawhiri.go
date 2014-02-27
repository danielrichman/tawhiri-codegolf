package tawhiri

import "time"

// Variables
type Position struct {
    Lat, Lon, Alt float64
}

type Delta struct {
    DLat, DLon, DAlt float64
}

func (p Position) Add(d Delta) (o Position) {
    o.Lat = p.Lat + d.DLat
    o.Lon = p.Lon + d.DLon
    o.Alt = p.Alt + d.DAlt
    return
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

func (d Delta) Add(e Delta) (o Delta) {
    o.DLat = d.DLat + e.DLat
    o.DLon = d.DLon + e.DLon
    o.DAlt = d.DAlt + e.DAlt
    return
}

func (d Delta) Scale(fac float64) (o Delta) {
    o.DLat = d.DLat * fac
    o.DLon = d.DLon * fac
    o.DAlt = d.DAlt * fac
    return
}

type Time struct {
    Now time.Time
    FlightTime float64
    ItemTime float64
}

func (t Time) Add(seconds float64) (o Time) {
    o.Now = t.Now.Add(time.Duration(seconds) * time.Second)
    o.FlightTime = t.FlightTime + seconds
    o.ItemTime = t.FlightTime + seconds
    return
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

func (c LinearCombination) Eval(state State) (o Delta) {
    for _, m := range c.Models {
        o = o.Add(m.Eval(state))
    }
    return
}

// Solvers
type Solver interface {
    Run(Model, TerminationCondition, State) []State
}

type Configuration struct {
    Model Model
    TerminationCondition TerminationCondition
    Solver Solver
}

func (c Configuration) Run(ics State) []State {
    return c.Solver.Run(c.Model, c.TerminationCondition, ics)
}

type Chain []Configuration

func (cs Chain) Run(ics State) (out []State) {
    out = append(out, ics)

    for _, c := range cs {
        // drop the first point (don't repeat it)
        out = append(out, c.Run(ics)[1:]...)

        ics = out[len(out) - 1]
        ics.ItemTime = 0
    }
    return
}

// Convenience functions
func InitialConditions(t time.Time, lat, lon, alt float64) State {
    return State{Time{t, 0, 0}, Position{lat, lon, alt}}
}

func MakeLinearCombination(models ...Model) LinearCombination {
    return LinearCombination{models}
}