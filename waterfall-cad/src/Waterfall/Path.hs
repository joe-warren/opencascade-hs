{-|
Paths in 3D space.
-}
module Waterfall.Path
( Path
, module Waterfall.Path.Common
, fromPath2D
-- $ reexports
, line3D
, lineTo3D
, lineRelative3D
, arcVia3D
, arcViaTo3D
, arcViaRelative3D
, bezier3D
, bezierTo3D
, bezierRelative3D
, pathFrom3D
, pathFromTo3D
) where

import Waterfall.Internal.Path (Path(..))


import Waterfall.Path.Common
import Waterfall.TwoD.Internal.Path2D (Path2D (..))
import Linear (V3)

-- | convert a `Path2D` into a `Path` on the \( xy \) plane (with \( z = 0 \) )
fromPath2D :: Path2D -> Path
fromPath2D (Path2D raw) = Path raw

-- $reexports
--
-- reexports from Waterfall.Path.Common, but monomorphised

-- | `line`, with the type fixed to `Path`
line3D :: V3 Double -> V3 Double -> Path
line3D = line 

-- | `lineTo`, with the type fixed to `Path`
lineTo3D :: V3 Double -> V3 Double -> (V3 Double, Path)
lineTo3D = lineTo

-- | `lineRelative`, with the type fixed to `Path`
lineRelative3D :: V3 Double -> V3 Double -> (V3 Double, Path)
lineRelative3D = lineRelative

-- | `arcVia`, with the type fixed to `Path`
arcVia3D :: V3 Double -> V3 Double -> V3 Double -> Path
arcVia3D = arcVia

-- | `arcViaTo`, with the type fixed to `Path`
arcViaTo3D :: V3 Double -> V3 Double -> V3 Double -> (V3 Double, Path)
arcViaTo3D = arcViaTo

-- | `arcViaRelative`, with the type fixed to `Path`
arcViaRelative3D :: V3 Double -> V3 Double -> V3 Double -> (V3 Double, Path)
arcViaRelative3D = arcViaRelative

-- | `bezier`, with the type fixed to `Path`
bezier3D :: V3 Double -> V3 Double -> V3 Double -> V3 Double ->  Path
bezier3D = bezier

-- | `bezierTo`, with the type fixed to `Path`
bezierTo3D :: V3 Double -> V3 Double -> V3 Double -> V3 Double ->  (V3 Double, Path)
bezierTo3D = bezierTo

-- | `bezierRelative`, with the type fixed to `Path`
bezierRelative3D :: V3 Double -> V3 Double -> V3 Double -> V3 Double ->  (V3 Double, Path)
bezierRelative3D = bezierRelative

-- | `pathFrom`, with the type fixed to `Path`
pathFrom3D :: V3 Double -> [V3 Double -> (V3 Double, Path)] -> Path
pathFrom3D = pathFrom

-- | `pathFromTo`, with the type fixed to `Path`
pathFromTo3D :: [V3 Double -> (V3 Double, Path)] -> V3 Double -> (V3 Double, Path)
pathFromTo3D = pathFromTo



