module GearExample
( gearExample
) where 

import qualified Waterfall.Solids as Solids
import qualified Waterfall.TwoD.Shape as Shape
import qualified Waterfall.TwoD.Path as Path
import Waterfall.TwoD.Internal.Path (joinPaths)
import Waterfall.TwoD.Transforms (rotate2D)
import Linear.V3
import Linear.V2
import Linear.Vector 
import Control.Lens ((^.))
import Data.Function ((&))
import Data.Maybe (catMaybes)
import Debug.Trace (traceShow)
-- This is directly implemented from "Gear Drawing with Bézier Curves" by Dr A R Collins 
-- https://www.arc.id.au/GearDrawing.html
-- found Indirectly via Mathew Dockrey (attoparsec (the YouTube channel, not the parser library))
-- https://github.com/attoparsec/inkscape-extensions
-- and David Douard and Jonas Bähr (FreeCAD)
-- https://github.com/FreeCAD/FreeCAD/blob/0ac0882eeb4e3390aef464e1807a3631c5f2e858/src/Mod/PartDesign/fcgear/involute.py


involuteBezCoeffs :: Double -> Int -> Double -> Double -> Double -> (V2 Double, V2 Double, V2 Double, V2 Double)
involuteBezCoeffs moduleLength nTeeth pressureAngle fStart fStop = 
    let 
        rPitch = moduleLength * (fromIntegral nTeeth) / 2
        phi = pressureAngle
        rB = rPitch * cos (phi)
        rA = rPitch + moduleLength
        p = 3
        ta = sqrt (rA * rA - rB * rB)/ rB -- involute angle at addendum
        ts = (sqrt fStart) * ta
        te = (sqrt fStop) * ta 
        chebyExpnCoeffs j f = 
            let n = 50 :: Int
                nf = fromIntegral n 
                c = sum [let kf = fromIntegral k in (cos (pi * (kf - 0.5)/nf) * cos (pi * j * (kf - 0.5)/nf))| k <- [1..n]]
                in 2 * c / nf

        chebyApprox f a b n = 
            let a' = 0.5 * (b - a)
                b' = 0.5 * (b + a)
                y = [ a' * cos (pi * (fromIntegral k + 0.5) / fromIntegral n) + b' | k <- [0..n-1] ]
                f' = map f y
            in [ 2 * sum (zipWith (*) f' [ cos (pi * fromIntegral j * (fromIntegral k + 0.5) / fromIntegral n) | k <- [0..n-1] ]) / fromIntegral n | j <- [0..n-1] ]

        involuteXbez t =
            let x = t*2 -1
                theta = x * (te - ts) / 2 + (ts + te)/2
            in rB * ( cos theta + theta * sin theta )
        involuteYbez t = 
            let x = t*2 - 1
                theta = x * (te - ts) / 2 + (ts + te)/2
            in rB * ( sin theta - theta * cos theta )

        binom n k = ((fromIntegral $ product [n - k + 1 .. n]) / (fromIntegral $ product [1..k]))

        bezCoeff i f = 
            let polyCoeffs = chebyApprox f fStart fStop (p+1)
            in traceShow polyCoeffs $ sum [binom i j * (polyCoeffs !! j) / binom p j | j<- [0..i]]

        v i = V2 (bezCoeff i involuteXbez) (bezCoeff i involuteYbez)

    in (v 0, v 1, v 2, v 3)

genInvolutePolar :: Double -> Double -> Double
genInvolutePolar rb r = (sqrt (r * r - rb * rb))/rb - acos (rb / r)

polarToCart :: Double -> Double -> V2 Double
polarToCart rad angle = V2 (rad * cos angle) (rad * sin angle)

rotateV2 :: V2 Double -> Double -> V2 Double 
rotateV2 (V2 x y) a = V2 (x * cos a - y * sin a) (x * sin a + y * cos a)

mirrorYV2 :: V2 Double -> V2 Double 
mirrorYV2 (V2 x y) = V2 x (-y)

genGearToothData :: Double -> Int -> Double -> Path.Path
genGearToothData m z phi = 
    let addendum = m
        dedendum = 1.25 * m
        clearance = dedendum - addendum
        rPitch = (fromIntegral z)  * m / 2
        rb = rPitch * cos phi
        ra = rPitch + addendum
        rRoot = rPitch - dedendum
        fRad = 1.5 * clearance 
        pitchAngle = 2 * pi / fromIntegral z
        baseToPitchAngle = genInvolutePolar rb rPitch
        rf' = sqrt ((rRoot + fRad) * (rRoot + fRad) - (fRad * fRad))
        rf = if rb < rf'
                then rRoot + clearance  
                else rf'
        pitchToFilletAngle  = 
            if rf > rb 
                then baseToPitchAngle - genInvolutePolar rb rf 
                else baseToPitchAngle 
        filletAngle = atan (fRad / (fRad + rRoot))
        fe = 1
        fs = if rf > rb 
            then (rf * rf - rb *rb) / (ra*ra - rb*rb)
            else 0.01 -- fraction of length offset from base to avoid singularity
        fm = fs + (fe - fs)/ 4
        (dbz1, dbz2, dbz3, dbz4) = involuteBezCoeffs m z phi fs fm 
        (_, abz2, abz3, abz4) = involuteBezCoeffs m z phi fs fm 
        rotateBez = (`rotateV2` (-baseToPitchAngle-pitchAngle/4))
        rotateBez' = mirrorYV2 . rotateBez
        fillet = polarToCart rf (-pitchAngle / 4 - pitchToFilletAngle)
        arcMiddle = polarToCart ra 0
        filletR = mirrorYV2 fillet
        rootR = polarToCart rRoot (pitchAngle/4 +pitchToFilletAngle + filletAngle)
        rootNext = polarToCart rRoot (3*pitchAngle/4 - pitchToFilletAngle - filletAngle)
        filletNext' = rotateV2 fillet (pitchAngle/2)
        filletNext = rotateV2 fillet pitchAngle

    in Path.pathFrom fillet $ 
        catMaybes 
            [ if rf < rb 
                then Just $ Path.lineTo (rotateBez dbz1)
                else Nothing
            , Just $ Path.bezierTo (rotateBez dbz2) (rotateBez dbz3) (rotateBez dbz4)
            , Just $ Path.bezierTo (rotateBez abz2) (rotateBez abz3) (rotateBez abz4)
            --, Just $ Path.arcTo (arcMiddle) (rotateBez' abz4)
            , Just $ Path.lineTo (rotateBez' abz4) -- this line should be an arc
            , Just $ Path.bezierTo (rotateBez' abz3) (rotateBez' abz2) (rotateBez' dbz4)
            , Just $ Path.bezierTo (rotateBez' dbz3) (rotateBez' dbz2) (rotateBez' dbz1)
            , if (rf < rb ) 
                then Just $ Path.lineTo filletR
                else Nothing
            , if (rootNext ^. _y > rootR ^. _y)
                then Just $ Path.pathFromTo 
                    [ Path.lineTo rootR
                    , Path.lineTo rootNext -- these lines should be arcs
                    ]
                else Nothing
            , Just $ Path.lineTo filletNext
        ]
-- Module, Number Teeth, pressure Angle 
gearExample :: Double -> Int -> Double -> Solids.Solid
gearExample moduleLength nGears pressureAngle =
    let segment = genGearToothData moduleLength nGears pressureAngle
        path = joinPaths [rotate2D (-fromIntegral n * pi * 2 / fromIntegral nGears) segment | n <- [0..nGears]]
    in Solids.prism 1 . Shape.fromPath $ path