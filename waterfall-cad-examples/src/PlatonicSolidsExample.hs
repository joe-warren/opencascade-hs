{-|
<<models/platonic.glb>>
-}
module PlatonicSolidsExample
( platonicSolidsExample
) where

import qualified Waterfall.Transforms as Transforms
import qualified Waterfall.Solids as Solids
import Linear ((^*), unit, _x)
platonicSolidsExample :: Solids.Solid
platonicSolidsExample = 
    mconcat . zipWith (Transforms.translate . (unit _x ^*)) [0..] $
        [ Solids.tetrahedron
        , Solids.centeredCube
        , Solids.octahedron
        , Solids.dodecahedron
        , Solids.icosahedron
        ]