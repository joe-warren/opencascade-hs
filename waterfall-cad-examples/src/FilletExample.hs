{-|
<<models/fillet.glb>>
-}
module FilletExample (
    filletExample
) where

import Waterfall.Solids( Solid, centeredCube )
import Waterfall.Transforms (translate)
import Waterfall.Fillet
    ( roundFillet
    , roundConditionalFillet
    , roundIndexedConditionalFillet
    , chamfer
    , conditionalChamfer
    , indexedConditionalChamfer
    )
import Control.Lens ((^.))
import Linear (V3 (..), _z)
import Control.Monad (guard)

filletExample :: Solid
filletExample =
    let gridLayout = 
            mconcat .
            zipWith 
                (\i -> translate (V3 i 0 0 ))
                [0, 2 ..]
                . fmap (
                    mconcat .
                    zipWith
                        (\i -> translate (V3 0 i 0 ))
                        [0, 2 ..]
            )
    in gridLayout 
         -- round every edge
        [[ roundFillet 0.1 centeredCube
            -- round horizontal edges 
         , roundConditionalFillet (\(s, e) -> if s ^. _z == e ^._z then Nothing else Just 0.1) centeredCube
         -- round edges with odd indices by a variable radius depending on the edge index
         , roundIndexedConditionalFillet (\i _ -> (fromIntegral i * 0.04) <$ guard (odd i)) centeredCube
        ], 
        -- chamfer every edge
        [ chamfer 0.1 centeredCube
         -- chamfer horizontal edges 
         , conditionalChamfer (\(s, e) -> if s ^. _z == e ^._z then Nothing else Just 0.1) centeredCube
         -- chamfer edges with odd indices by a variable amount depending on the edge index
         , indexedConditionalChamfer (\i _ -> (fromIntegral i * 0.04) <$ guard (odd i)) centeredCube
        ]]
