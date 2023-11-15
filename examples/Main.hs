module Main (main) where

import CsgExample (csgExample)
import Waterfall.IO (writeSTL)
main :: IO ()
main = writeSTL 0.001 "csg-example.stl" csgExample
