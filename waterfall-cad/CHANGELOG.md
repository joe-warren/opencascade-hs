# Changelog for `waterfall-cad`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).


## Unreleased

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
