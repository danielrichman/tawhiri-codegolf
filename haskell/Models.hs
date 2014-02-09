module Models
( Model
, Model3D
, Model2D
, AltitudeProfile
, TerminationCondition
, linearCombination
, fromHorizontalModel
) where

import Data.Monoid

import Variables

type Model a = Position -> Time -> a
type Model3D = Model Delta3D
type Model2D = Model Delta2D
type AltitudeProfile = Time -> Altitude
type TerminationCondition = Position -> Time -> Bool

linearCombination :: (Delta a) => [Model a] -> Model a
linearCombination models posn time =
    mconcat $ map (\m -> m posn time) models

fromHorizontalModel :: Model2D -> Model3D
fromHorizontalModel model p t = fromDelta2D $ model p t
