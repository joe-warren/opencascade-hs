module FilletExample (
    filletExample
) where

import Waterfall.Solids( Solid, centeredCube )
import Waterfall.Transforms (translate)
import Waterfall.Fillet(roundFillet, roundConditionalFillet, roundIndexedConditionalFillet)
import Control.Lens ((^.))
import Linear (_z, V3 (..))
import Control.Monad (guard)

filletExample :: Solid
filletExample =
    mconcat $ zipWith
    (\i -> translate (V3 i 0 0 ))
     [0, 2 ..]
     [roundFillet 0.1 centeredCube
     , roundConditionalFillet (\(s, e) -> if s ^. _z == e ^._z then Nothing else Just 0.1) centeredCube
     , roundIndexedConditionalFillet (\i _ -> (fromIntegral i * 0.01) <$ guard (even i)) centeredCube
     ]

