module ForwardsEuler ( forwardsEuler, forwardsEulerWithAP ) where

import Variables
import Solvers

forwardsEuler :: Double -> Solver
forwardsEuler h model termcond initconds = start : step start
    where start = (position initconds, timeAfterICs initconds 0)
          step (x, t) =
            if termcond x t then [] else
                let x_dot = model x t
                    next = (wrapLongitude $ x .+ x_dot .* h, t ..+ h)
                in
                    next : step next

forwardsEulerWithAP :: Double -> SolverWithAP
forwardsEulerWithAP h model altprof termcond initconds = start : step start
    where start = (position initconds, timeAfterICs initconds 0)
          step (x, t) =
            if termcond x t then [] else
                let x_dot = model x t
                    x' = replaceAltitude (x .+ x_dot .* h) $ altprof t
                    next = (wrapLongitude x', t ..+ h)
                in
                    next : step next
