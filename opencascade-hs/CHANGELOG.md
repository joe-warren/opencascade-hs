# Changelog for `opencascade-hs`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

### Added 

- Added OpenCascade.BRepPrimAPI.MakeCone

## 0.1.1.1 - 2023-12-12 

### Fixed 

- Include header files in package, fixing build

## 0.1.1.0 - 2023-12-12 

### Added

- Add OpenCascade.BRepOffsetAPI.MakeOffsetShape
- Add OpenCascade.GeomAbs.JoinType
- Add OpenCascade.BRepOffset.Mode
- Add `dn` to OpenCascade.Geom.Curve

## 0.1.0.0 - 2023-12-05 

### Added 

- Add OpenCascade.Font, including:
    - Add OpenCascade.Font.BRepFont
    - Add OpenCascade.Font.BRepTextBuilder
    - Add OpenCascade.Font.FontAspect
    - Add OpenCascade.Graphic3D.HorizontalTextAlignment
    - Add OpenCascade.Graphic3D.VerticalTextAlignment
- Add OpenCascade.GP.Ax3

### Fixed

- Fix build on MacOS (tested with the homebrew install of OpenCASCADE)
- Fix OpenCascade.TopoDS.Shape.copy

## 0.0.0.1 - 2023-11-29
