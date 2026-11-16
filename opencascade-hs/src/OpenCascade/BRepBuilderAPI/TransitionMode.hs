module OpenCascade.BRepBuilderAPI.TransitionMode
( TransitionMode (..)
) where

-- this should match the enumeration in BRepBuilderAPI_TransitionMode
data TransitionMode = Transformed | RightCorner | RoundCorner
    deriving (Show, Eq, Ord, Enum)