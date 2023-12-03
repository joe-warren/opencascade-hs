module OpenCascade.Font.FontAspect
( FontAspect (..)
) where

data FontAspect = Undefined | Regular | Bold | Italic | BoldItalic deriving (Enum, Show)