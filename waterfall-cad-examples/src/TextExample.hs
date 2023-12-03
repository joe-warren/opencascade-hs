module TextExample (
    textExample
) where

import qualified Waterfall

textExample :: FilePath -> Double -> String -> Double -> Waterfall.Solid
textExample fontpath fontSize content depth = 
    let font = Waterfall.fontFromPath fontpath fontSize
    in Waterfall.prism depth $ Waterfall.text font content