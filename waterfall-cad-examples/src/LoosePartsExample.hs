{-|
<<models/loose-parts.glb>>
-}
module LoosePartsExample
( loosePartsExample
) where

import Waterfall.Solids (Solid, looseParts)
import qualified Waterfall.Solids as Solids
import Waterfall.Colour (Colour)
import qualified Waterfall.Colour as Colour
import qualified Waterfall.Transforms as Transforms
import qualified Waterfall.Booleans as Booleans
import Linear (V3 (..), unit, _x, _z, (^*))
import Data.Function ((&))

baseCube :: Solid
baseCube = Solids.centeredCube

colours :: [Colour]
colours = [Colour.red, Colour.fuchsia, Colour.blue, Colour.aqua, Colour.lime, Colour.yellow, Colour.white]

separator :: Solid
separator =
    let n = length colours
    in Solids.centeredCube 
        & Transforms.translate (unit _x ^* 0.5)
        & Transforms.scale (V3 2 0.1 2)
        & iterate (Transforms.rotate (unit _z) (2 * pi / realToFrac n))
        & take n
        & mconcat

loosePartsExample :: Solid
loosePartsExample = 
    (baseCube `Booleans.difference` separator)
        & looseParts
        & zipWith Solids.colourSolid colours
        & mconcat