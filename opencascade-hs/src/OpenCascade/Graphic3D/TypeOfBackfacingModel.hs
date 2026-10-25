module OpenCascade.Graphic3D.TypeOfBackfacingModel
( TypeOfBackfacingModel (..)
) where

-- Should match the values in Graphic3d_TypeOfBackfacingModel.hxx
data TypeOfBackfacingModel = Auto | DoubleSided | BackCulled | FrontCulled deriving (Show, Eq, Enum)
