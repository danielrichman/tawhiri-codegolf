package main

import (
    "fmt"
    "time"
    "./tawhiri"
    "./tawhiri/dataset"
    "./tawhiri/models"
    "./tawhiri/solvers"
)

func main() {
    ds_time := time.Date(2014, 02, 18, 12, 0, 0, 0, time.UTC)
    ds := dataset.Open(ds_time, "/opt/wind")

    model := tawhiri.MakeLinearCombination(models.Wind{ds}, models.Linear{5})
    termcond := models.BurstAltitude{30000}
    solver := solvers.ForwardsEuler{1}
    config := tawhiri.Configuration{model, termcond, solver}
    chain := tawhiri.Chain{config}
    launch_time := time.Date(2014, 02, 19, 15, 0, 0, 0, time.UTC)
    ics := tawhiri.InitialConditions(launch_time, 52.2135, 0.0964, 0)

    pipe := make(chan tawhiri.State, 100)
    out := make(chan tawhiri.State, 100)

    go chain.Run(ics, pipe)
    go tawhiri.Decimate(60, pipe, out)

    fmt.Printf("var data = [")

    point := <-out
    fmt.Printf("[%.15f, %.15f]", point.Lat, point.Lon)
    for point := range out {
        fmt.Printf(",[%.15f, %.15f]", point.Lat, point.Lon)
    }

    fmt.Printf("];\n")
}
