module Waterfall.Internal.NearZero 
( confusion
, nearZero
) where

confusion :: Double
confusion = 1e-7

nearZero :: Double -> Bool
nearZero = (<= confusion) . abs