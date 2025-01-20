module OpenCascade.GeomFill.Trihedron
( Trihedron (..)
) where

-- this should match the enumeration in GeomFill_Trihedron
data Trihedron =  IsCorrectedFrenet | IsFixed | IsFrenet | IsConstantNormal |
  IsDarboux | IsGuideAC | IsGuidePlan | IsGuideACWithContact |
  IsGuidePlanWithContact | IsDiscreteTrihedron deriving (Show, Eq, Enum)