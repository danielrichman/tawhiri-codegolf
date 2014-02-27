import Data.Time.Clock()
import Data.Binary (decode)
import qualified Data.ByteString.Lazy as B
import Dataset (open, interpolate, get)
import Variables

main :: IO ()
main = do
    dataset <- open "/mnt/data/temp" $ read "2014-02-03 06:00:00"
    putStrLn . show $ get dataset (0,0,0,284,2)
    putStrLn . show $ interpolate dataset (Position 52 1 100) (Time (read "2014-02-03 09:00:01") 0 0)


