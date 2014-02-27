package models

import "../../tawhiri"

type ConstantAltitude struct {
}

func (c ConstantAltitude) Eval(_ tawhiri.State) (out tawhiri.Delta) {
    return
}

type Linear struct {
    Speed float64
}

func (l Linear) Eval(_ tawhiri.State) (out tawhiri.Delta) {
    out.DAlt = l.Speed
    return
}
