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
    start := time.Now()

    ds_time := time.Date(2014, 02, 18, 12, 0, 0, 0, time.UTC)
    ds := dataset.Open(ds_time, "/opt/wind")

    model := tawhiri.MakeLinearCombination(models.Wind{ds}, models.Linear{5})
    termcond := models.BurstAltitude{30000}
    solver := solvers.ForwardsEuler{1}
    config := tawhiri.Configuration{model, termcond, solver}
    chain := tawhiri.Chain{config}
    launch_time := time.Date(2014, 02, 19, 15, 0, 0, 0, time.UTC)
    ics := tawhiri.InitialConditions(launch_time, 52.2135, 0.0964, 0)

    fmt.Println("Setup took", time.Now().Sub(start))

    loopstart := time.Now()

    for i := 0; i < 100; i++ {
        for _ = range chain.Run(ics) {
        }
    }

    took := time.Now().Sub(loopstart)

    fmt.Println("100 iterations", took, "total", took / 100, "avg")
}
