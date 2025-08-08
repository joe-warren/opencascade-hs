{-|
<<models/bounding-boxes.glb>>
-}
module BoundingBoxExample
( boundingBoxExample
) where

import Data.Function ((&))
import qualified Waterfall.Solids as Solids
import qualified Waterfall.BoundingBox.Oriented as OBB
import qualified Waterfall.BoundingBox.AxisAligned as AABB
import qualified Waterfall.Transforms as Transforms
import Linear (V3 (..), unit, _y, _z, (^*))

boundingBoxExample :: Solids.Solid
boundingBoxExample = 
    let shape = Solids.unitSphere &
            Transforms.scale (V3 0.5 3 5) &
            Transforms.rotate (unit _z) 1 &
            Transforms.rotate (unit _y) 0.5
        obb = OBB.orientedBoundingBox shape &
            maybe Solids.nowhere OBB.obbToSolid
        aabb = AABB.axisAlignedBoundingBox shape &
            maybe Solids.nowhere AABB.aabbToSolid
        in mconcat 
            [ Transforms.translate (unit _y ^* (-5)) shape
            , obb
            , Transforms.translate (unit _y ^* 5) aabb
            ]   