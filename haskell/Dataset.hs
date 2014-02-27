module Dataset
( Dataset
, open
, interpolate
, get
) where

import Data.Time.Clock
import Data.Time.Format
import Data.Vector.Storable (Vector(..), unsafeFromForeignPtr, (!))
import Control.Exception (assert)
import Foreign.Ptr
import Foreign.Storable
import System.IO.MMap
import System.FilePath.Posix (combine)
import System.Locale

import Variables

data Dataset = Dataset { vector :: Vector Double, time :: UTCTime }
type Index = Int
type Subscript = (Index, Index, Index, Index, Index)

shape :: Subscript
shape = (65, 47, 3, 361, 720)
arrayLen :: Index
arrayLen = a * b * c * d * e
    where (a, b, c, d, e) = shape

data Variable = Height | WindU | WindV deriving (Ord, Eq, Read, Show)

variableIdx :: Variable -> Index
variableIdx v = case v of Height -> 0
                          WindU -> 1
                          WindV -> 2

offset :: Subscript -> Index
offset subscr =
    let (sza, szb, szc, szd, sze) = shape
        (ida, idb, idc, idd, ide) = subscr
        ida' = chkcnv sza ida
        idb' = chkcnv szb idb
        idc' = chkcnv szc idc
        idd' = chkcnv szd idd
        ide' = chkcnv sze ide
        off = ide' + sze * (idd' + szd * (idc' + szc * (idb' + szb * ida')))
    in
        assert (0 <= off && off < arrayLen) off
    where chkcnv size idx = assert (idx >= 0 && idx < size) idx

get :: Dataset -> Subscript -> Double
get (Dataset vec _) subscr = vec ! offset subscr

fileName :: String -> UTCTime -> String
fileName dir t = combine dir (formatTime defaultTimeLocale "%Y%m%d%H" t)

open :: String -> UTCTime -> IO Dataset
open directory dstime = do
    let dsfrac = floor $ realToFrac $ utctDayTime dstime
        dstime' = assert (dsfrac `mod` (3600 * 6) == 0) dstime
        filename = fileName directory dstime'
    (foreignPtr, offset, size) <- mmapFileForeignPtr filename ReadOnly Nothing
    let size' = assert (size == arrayLen * sizeOf (undefined :: Double)) size
        vector = unsafeFromForeignPtr foreignPtr offset arrayLen
    return (Dataset vector dstime')

interpolate :: Dataset -> Position -> Time -> (Double, Double)
interpolate dset (Position latitude longitude altitude) Time { now=now' } =
    let hour = realToFrac $ (now' `diffUTCTime` time dset) / 3600
        idlps = idxLerps3 hour latitude longitude
        interp3' = interp3 idlps
        (_, szalt, _, _, _) = shape
        level = search 0 (szalt - 1) altitude $ interp3' Height
        (lower, upper) = if level == 0 then (0, 1)
                         else if level == szalt then (szalt - 2, szalt - 1)
                         else (level - 1, level)
        lowerAlt = interp3' Height lower
        upperAlt = interp3' Height upper
        altStep = (upperAlt - lowerAlt)
        lerp = if altStep == 0 then 0.5 else (altitude - lowerAlt) / altStep
        interp4' = interp4 idlps lower upper lerp
    in
        (interp4' WindU, interp4' WindV)
    where
        interp3 idlps variable level =
            let get2 lvl var ((ai, bi, ci), lerp) =
                    get dset (ai, lvl, variableIdx var, bi, ci) * lerp
            in
                foldl1 (+) $ map (get2 level variable) idlps

        interp4 idlps lower upper lerp variable =
            let f = interp3 idlps variable in
            f lower * (1 - lerp) + f upper * lerp

        search lower upper find func =
                if lower == upper then lower
                else if find <= test then search lower mid find func
                else search (mid + 1) upper find func
            where mid = (lower + upper) `div` 2
                  test = func mid

-- (indexes, product of lerps)
idxLerps3 :: Double -> Double -> Double -> [((Index, Index, Index), Double)]
idxLerps3 hour latitude longitude = do
        (ai, al) <- pickhr hour
        (bi, bl) <- picklat latitude
        (ci, cl) <- picklon longitude
        return ((ai, bi, ci), al * bl * cl)
    where
        (szhr, _, _, szlat, szlon) = shape

        pick left step n value =
            let a = (value - left) / step :: Double
                b = floor a :: Index
                b2 = assert (0 <= b && b < n - 1) b
                lerp = a - fromIntegral b2
            in
                [(b, 1 - lerp), (b + 1, lerp)]
        pickhr = pick 0 3 $ fromIntegral szhr
        picklat = pick (-90) 0.5 $ fromIntegral szlat
        picklon value =
            let sz = fromIntegral szlon + 1
                [left, (ridx, rlerp)] = pick 0 0.5 sz value
                ridx2 = if ridx == szlon then 0 else ridx
            in [left, (ridx2, rlerp)]
