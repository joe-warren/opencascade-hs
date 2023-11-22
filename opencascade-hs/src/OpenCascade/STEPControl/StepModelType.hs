module OpenCascade.STEPControl.StepModelType
( StepModelType(..)
) where

-- this should match the order defined in StepModelType
data StepModelType = 
    Asls |
    ManifoldSolidBrep |
    BrepWithVoids | 
    FacetedBrep |
    FacetedBrepAndBrepWithVoids |
    ShellBasedSurfaceModel |
    GeometricCurveSet |
    Hybrid 
    deriving Enum