# Changelog for `waterfall-cad`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).


## Unreleased

- Add `Waterfall.Loft` containing `loft` and `pointedLoft`
- Change the `Monoid` instance for `Path` and `Path2D`, so that in the expression `a <> b` a line is added between the end of `a` and the start of `b`, unless these points are coincident.
- Reverse the order in which Path.pathFrom adds path segments; required by the new Monoid behaviour.
- Add `Waterfall.Path.Common.reversePath`, reversing the direction of a `Path` or `Path2D`, along with monomorphised versions `reversePath3D` and `reversePath2D`

## 0.3.0.1

### Added

- Add `pathEndpoints`, `pathEndpoints2D` and `pathEndpoints3D`

## 0.3.0.0

### Changed

- The project now depends on (and supports) OpenCascade `7.8.0` or later

## 0.2.2.1

### Fixed

- Importing files that contain multiple separate solids should no longer error

## 0.2.2.0

### Added 
- New functions in `Waterfall.IO`
    - Support for reading (as well as writing) all file formats
        - `readSTL`, `readSTEP`, `readGLTF`, `readGLB`, `readOBJ`
    - (Wavefront) OBJ support (`writeOBJ`)
    - Convenience methods `readSolid` and `writeSolid` inferring the format from the file extension

### Changed

- Make error handling for actions in `Waterfall.Text` throw a `WaterfallIOException` on failure
## 0.2.1.0

### Added

- GLTF (graphics library transmission format) support: `writeGLTF` and `writeGLB` in `Waterfall.IO`
- obj file format support `writeOBJ` in `Waterfall.IO`

## 0.2.0.0

### Added 

- the following `volume`, `momentOfInertia` and `centerOfMass` queries in `Waterfall.Solids`
    - `volume`
    - `momentOfInertia`
    - `centerOfMass`
- `aabbToSolid` to `Waterfall.Solids` (converts the output of `axisAlignedBoundingBox` into a solid)
- `Waterfall.BoundingBox.AxisAligned`, for calculating (and reifying) an (axis aligned) bounding box of a solid 
- `Waterfall.BoundingBox.Oriented`, for calculating (and reifying) an (oriented) bounding box of a solid 

### Changed 

- Changed the representation of `Solid` (and other Waterfall values) from a newtype wrapper to `Data.Acquire` to a naked `Ptr`, with destructors called using Finalizers.
    - This means it's possible to support "queries", like calculating the volume of a Solid
    - It also means there's some risk of exceptions being thrown when working with plain values, which wasn't present before
- `Waterfall.Text.fontFromPath` and `Waterfall.Text.fontFromSystem` now return `IO` actions rather than embedding the action into the underlying `Shape`

### Fixed

- Typo in documentation for `Waterfall.Solids.unitCone` 

## 0.1.2.2 - 2024-01-09 

### Fixed

- Fixed dependency versions

## 0.1.2.1 - 2024-01-09 

### Added

- Added `mirror` to `Transformable` typeclass, and `mirror2D` to `Transformable2D`
- Added `fromPath2D` to `Waterfall.Path` to make 2D paths into 3D ones.
- Added `centeredCylinder` and `unitCone` to `Waterfall.Solids`

### Fixed

- Handle offsetting by zero (return the unmodified shape)
- Fix rotation of endcaps in a `sweep`

## 0.1.1.1 - 2023-12-12 

## 0.1.1.0 - 2023-12-12 

### Added 

- Add Waterfall.Offset, offsetting an object by a certain amount
- Add `unitCircle`, `unitSquare` and `centeredSquare` to `Waterfall.TwoD.Shape`

### Fixed

- Correctly rotate and translate endcaps to the `Path` in a `sweep`

## 0.1.0.0 - 2023-12-05 

### Added

- Reexported all modules under top level `Waterfall` module
- Refactored common code in `Path` and `Path2D` into the `AnyPath` typeclass
- Added Waterfall.Text, containing text rendering functions

### Fixed

- Fix build on MacOS (tested with the homebrew install of OpenCASCADE)


## 0.0.0.1 - YYYY-MM-DD
