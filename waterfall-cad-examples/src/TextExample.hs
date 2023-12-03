module TextExample (
    textExample
) where

import qualified Waterfall

textExample :: FilePath -> Double -> String -> Double -> Waterfall.Solid
textExample fontpath fontSize content depth = Waterfall.prism depth $ Waterfall.text fontpath fontSize content