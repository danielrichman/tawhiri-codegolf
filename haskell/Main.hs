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

toDataJS :: [(Position, Time)] -> String
toDataJS points = showJSArray (map restructure points) ""
    where restructure ((Position lat lon _), _) = JSArray [JSRational True $ realToFrac lat, JSRational True $ realToFrac lon]

main :: IO ()
main = do
    dataset <- open "/mnt/data/temp" $ read "2014-02-03 06:00:00"
    let model = linearCombination [wind dataset]
        ap = linear 0 5
        termcond = burstAltitude 30000
        solver = forwardsEulerWithAP 1
        config = solver model ap termcond
        launch = read "2014-02-03 09:00:00"
        ics = InitialConditions (Position 52.2135 0.0964 0) launch 0
        soln = chain [config] ics
        decimated = decimateNth 60 soln
    putStr "var data = "
    putStr $ toDataJS decimated
    putStrLn ";"
    -- mapM_ (putStrLn . show) decimated
    -- mapM_ (putStrLn . show . (\(Position lat lon _, _) -> [lat, lon])) decimated
