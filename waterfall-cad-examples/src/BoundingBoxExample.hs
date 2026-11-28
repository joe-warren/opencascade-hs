{-|
<<models/bounding-boxes.glb>>
-}
module BoundingBoxExample
( boundingBoxExample
) where

import Data.Function ((&))
import qualified Waterfall.Solid as Solid
import qualified Waterfall.BoundingBox.Oriented as OBB
import qualified Waterfall.BoundingBox.AxisAligned as AABB
import qualified Waterfall.Transforms as Transforms
import qualified Waterfall.Colour as Colour
import Linear (V3 (..), unit, _y, _z, (^*))

boundingBoxExample :: Solid.Solid
boundingBoxExample = 
    let shape = Solid.unitSphere 
            & Transforms.scale (V3 0.5 3 5) 
            & Transforms.rotate (unit _z) 1 
            & Transforms.rotate (unit _y) 0.5
            & Solid.colourSolid Colour.red
        obb = OBB.orientedBoundingBox shape 
            & maybe Solid.emptySolid OBB.obbToSolid
            & Solid.colourSolid Colour.aqua
        aabb = AABB.axisAlignedBoundingBox shape 
            & maybe Solid.emptySolid AABB.aabbToSolid
            & Solid.colourSolid Colour.lime
        in mconcat 
            [ Transforms.translate (unit _y ^* (-5)) shape
            , obb
            , Transforms.translate (unit _y ^* 5) aabb
            ]   