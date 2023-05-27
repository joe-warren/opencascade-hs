module OpenCascade.BRepBuilderAPI.FaceError
( FaceError (..)
) where

-- order must match the definition of BRepBuilderAPI FaceError
data FaceError = FaceDone | NoFace | NotPlanar | CurveProjectionFailed | ParametersOutOfRange deriving (Eq, Enum)
