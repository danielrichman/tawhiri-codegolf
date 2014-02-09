module Solvers
( Solver
, SolverWithAP
, Configuration
, chain
, decimateNth
) where

import Variables
import Models

type Solver = Model3D -> TerminationCondition ->
                InitialConditions -> [(Position, Time)]
type SolverWithAP = Model2D -> AltitudeProfile ->
                TerminationCondition ->
                InitialConditions -> [(Position, Time)]

type Configuration = InitialConditions -> [(Position, Time)]

chain :: [Configuration] -> Configuration
chain [] _ = []
chain (c:cs) initial_conditions =
    let points = c initial_conditions
        (last_posn, Time { now=last_dt, flightTime=last_ft }) = last points
        next_ics = InitialConditions last_posn last_dt last_ft
    in points ++ (drop 1 $ chain cs next_ics)

decimateNth :: Int -> [(Position, Time)] -> [(Position, Time)]
decimateNth _ [] = []
decimateNth _ [x] = [x]
decimateNth n (x:xs) = case drop (n - 1) xs of
                        [] -> [x, last xs] -- always keep the last point
                        others -> x : decimateNth n others
