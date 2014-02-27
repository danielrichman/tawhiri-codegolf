package solvers

import "../../tawhiri"

type ForwardsEuler struct {
    Step float64
}

func (fe ForwardsEuler) Run(model tawhiri.Model, tc tawhiri.TerminationCondition, ics tawhiri.State, out chan tawhiri.State) {
    state := ics
    out <- state

    for !tc.Eval(state) {
        x_dot := model.Eval(state).Scale(fe.Step)
        state = tawhiri.State{state.Time.Add(fe.Step), state.Position.Add(x_dot)}
        state.Lon = tawhiri.WrapLongitude(state.Lon)
        out <- state
    }

    close(out)
}

