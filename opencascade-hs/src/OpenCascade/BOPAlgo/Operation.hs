module OpenCascade.BOPAlgo.Operation
( Operation (..)
) where

-- this should match the enumeration in BOPAlgo_Operation
data Operation = Common | Fuse | Cut | Cut21 | Section | Unknown
    deriving (Show, Eq, Enum)