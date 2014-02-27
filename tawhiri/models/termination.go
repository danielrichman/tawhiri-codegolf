package models

import "../../tawhiri"

type BurstAltitude struct {
    Alt float64
}

func (b BurstAltitude) Eval(s tawhiri.State) bool {
    return s.Alt >= b.Alt
}

type LandedMSL struct {
}

func (_ LandedMSL) Eval(s tawhiri.State) bool {
    return s.Alt <= 0
}

type Timeout struct {
    Seconds float64
}

func (to Timeout) Eval(s tawhiri.State) bool {
    return s.ItemTime >= to.Seconds
}
