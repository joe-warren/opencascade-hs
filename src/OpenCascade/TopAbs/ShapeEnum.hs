module OpenCascade.TopAbs.ShapeEnum
( ShapeEnum (..)
) where

-- this should match the enumeration in TopAbs_ShapeEnum
data ShapeEnum = Compound | CompSolid | Solid | Shell | Face | Wire | Edge | Vertex | Shape deriving Enum
