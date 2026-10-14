{-|
<<models/platonic.glb>>
-}
module PlatonicSolidsExample
( platonicSolidsExample
) where

import qualified Waterfall.Transforms as Transforms
import qualified Waterfall.Solids as Solids
import qualified Waterfall.Colour as Colour
import Linear ((^*), unit, _y)
platonicSolidsExample :: Solids.Solid
platonicSolidsExample = 
    mconcat . zipWith (Transforms.translate . (unit _y ^*)) [0, -2.5 ..] $
        [ Solids.colourSolid Colour.red Solids.tetrahedron
        , Solids.colourSolid Colour.yellow Solids.centeredCube
        , Solids.colourSolid Colour.lime Solids.octahedron
        , Solids.colourSolid Colour.aqua Solids.icosahedron
        , Solids.colourSolid Colour.blue Solids.dodecahedron
        ]