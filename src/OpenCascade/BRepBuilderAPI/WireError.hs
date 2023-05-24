module OpenCascade.BRepBuilderAPI.WireError
( WireError (..)
) where

-- order must match the definition of BRepBuilderAPI_WireError
data WireError = WireDone | EmptyWire | DisconnectedWire | NonManifoldWire deriving (Eq, Enum)
