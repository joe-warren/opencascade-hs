module OpenCascade.GeomAbs.JoinType
( JoinType (..)
) where

-- This needs to stay up to date with GeomAbs_JoinType.hxx
data JoinType = Arc | Tangent | Intersection deriving (Enum, Show)