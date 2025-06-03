module Main 
( main
) where 

import qualified Waterfall
import Criterion
import Criterion.Main (defaultConfig, defaultMainWith)
import Linear 
import Data.Function ((&))
import Criterion.Types (Config(resamples))

benchmarkAction :: ([Waterfall.Solid] -> Waterfall.Solid) -> Int -> IO ()
benchmarkAction combineF size = 
    let s = [ Waterfall.unitSphere &
                Waterfall.translate (V3 x y 0)
            | x <- fromIntegral <$> [0..size]
            , y <- fromIntegral <$> [0..size] 
            ]
    in Waterfall.writeSTL 0.1 "output.stl" (combineF s)

main :: IO ()
main = defaultMainWith (defaultConfig {resamples = 10})
    [ bench "unions" $ whnfAppIO (benchmarkAction Waterfall.unions) 15
    , bench "mconcat" $ whnfAppIO (benchmarkAction mconcat) 15
    , bench "foldr (old mconcat)" $ whnfAppIO (benchmarkAction (foldr mappend mempty)) 15
    ]