{-|
<<models/fillet.glb>>
-}
module FilletExample (
    filletExample
) where

import Waterfall.Solids( Solid, centeredCube )
import Waterfall.Transforms (translate)
import Waterfall.Fillet(roundFillet, roundConditionalFillet, roundIndexedConditionalFillet)
import Control.Lens ((^.))
import Linear (V3 (..), _z)
import Control.Monad (guard)

filletExample :: Solid
filletExample =
    mconcat $ zipWith
        (\i -> translate (V3 i 0 0 ))
        [0, 2 ..]
        -- round every edge
        [ roundFillet 0.1 centeredCube
        -- round horizontal edges 
        , roundConditionalFillet (\(s, e) -> if s ^. _z == e ^._z then Nothing else Just 0.1) centeredCube
        -- round edges with odd indices by a variable radius depending on the edge index
        , roundIndexedConditionalFillet (\i _ -> (fromIntegral i * 0.04) <$ guard (odd i)) centeredCube
        ]

