{-|
<<models/platonic.glb>>
-}
module PlatonicSolidsExample
( platonicSolidsExample
) where

import qualified Waterfall.Transforms as Transforms
import qualified Waterfall.Solids as Solids
import Linear ((^*), unit, _y)
platonicSolidsExample :: Solids.Solid
platonicSolidsExample = 
    mconcat . zipWith (Transforms.translate . (unit _y ^*)) [0, -2.5 ..] $
        [ Solids.tetrahedron
        , Solids.centeredCube
        , Solids.octahedron
        , Solids.icosahedron
        , Solids.dodecahedron
        ]