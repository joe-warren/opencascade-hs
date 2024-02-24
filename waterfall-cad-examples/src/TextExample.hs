module TextExample (
    textExample
) where

import qualified Waterfall

textExample :: FilePath -> Double -> String -> Double -> IO Waterfall.Solid
textExample fontpath fontSize content depth = do
    font <- Waterfall.fontFromPath fontpath fontSize
    return . Waterfall.prism depth $ Waterfall.text font content