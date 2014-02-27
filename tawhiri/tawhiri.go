package tawhiri

import "time"

// Variables
type Position struct {
    Lat, Lon, Alt float64
}

type Delta struct {
    DLat, DLon, DAlt float64
}

func (p *Position) Add(d Delta) {
    p.Lat += d.DLat
    p.Lon += d.DLon
    p.Alt += d.DAlt
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

func (d *Delta) Add(e Delta) {
    d.DLat += e.DLat
    d.DLon += e.DLon
    d.DAlt += e.DAlt
    return
}

func (d *Delta) Scale(fac float64) {
    d.DLat *= fac
    d.DLon *= fac
    d.DAlt *= fac
}

type Time struct {
    Now time.Time
    FlightTime float64
    ItemTime float64
}

func (t *Time) Add(seconds float64) {
    t.Now = t.Now.Add(time.Duration(seconds) * time.Second)
    t.FlightTime += seconds
    t.ItemTime += seconds
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
        o.Add(m.Eval(state))
    }
    return
}

// Solvers
type Solver interface {
    Run(Model, TerminationCondition, State, chan State)
}

type Configuration struct {
    Model Model
    TerminationCondition TerminationCondition
    Solver Solver
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
        if i % n == 0 {
            out <- point
        }
        i += 1
    }
    // always emit the last point
    if i % n != 1 {
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
