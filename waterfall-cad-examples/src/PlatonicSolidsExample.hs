{-|
<<models/platonic.glb>>
-}
module PlatonicSolidsExample
( platonicSolidsExample
) where

import qualified Waterfall.Transforms as Transforms
import qualified Waterfall.Solid as Solid
import qualified Waterfall.Colour as Colour
import Linear ((^*), unit, _y)
platonicSolidsExample :: Solid.Solid
platonicSolidsExample = 
    mconcat . zipWith (Transforms.translate . (unit _y ^*)) [0, -2.5 ..] $
        [ Solid.colourSolid Colour.red Solid.tetrahedron
        , Solid.colourSolid Colour.yellow Solid.centeredCube
        , Solid.colourSolid Colour.lime Solid.octahedron
        , Solid.colourSolid Colour.aqua Solid.icosahedron
        , Solid.colourSolid Colour.blue Solid.dodecahedron
        ]