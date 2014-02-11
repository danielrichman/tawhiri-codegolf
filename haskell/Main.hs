import Data.Time.Clock()
import Text.JSON

import Dataset (open)
import ForwardsEuler (forwardsEulerWithAP)
import Models (linearCombination)
import Solvers (chain, decimateNth)
import Simple.Wind (wind)
import Simple.AltitudeProfiles (linear)
import Simple.TerminationConditions (burstAltitude)
import Variables (InitialConditions(..), Position(..), Time)


toJS :: [(Position, Time)] -> IO ()
toJS points = do
        putStr "var data = "
        putStr $ showJSArray (map restructure points) ""
        putStrLn ";"
    where restructure ((Position lat lon _), _) = JSArray [r lat, r lon]
          r v = JSRational False $ realToFrac v

main :: IO ()
main = do
    let dir = "/var/www/predict/tawhiri/datasets/"
    dataset <- open dir $ read "2014-02-09 12:00:00"
    let model = linearCombination [wind dataset]
        ap = linear 0 5
        termcond = burstAltitude 30000
        solver = forwardsEulerWithAP 1
        config = solver model ap termcond
        launch = read "2014-02-09 21:00:00"
        ics = InitialConditions (Position 52.2135 0.0964 0) launch 0
        soln = chain [config] ics
        decimated = decimateNth 60 soln

    toJS decimated
