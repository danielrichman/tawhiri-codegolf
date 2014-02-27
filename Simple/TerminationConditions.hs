module Simple.TerminationConditions ( landedMSL, burstAltitude, timeout) where

import Variables
import Models

landedMSL :: TerminationCondition
landedMSL (Position _ _ alt) _ = alt <= 0

burstAltitude :: Altitude -> TerminationCondition
burstAltitude burst_alt (Position _ _ alt) _ = alt >= burst_alt

timeout :: Double -> TerminationCondition
timeout seconds _ time = itemTime time > seconds
