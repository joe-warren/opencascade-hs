# Changelog for `opencascade-hs`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

## 0.6.0.0

- Add `OpenCascade.BRepFiletAPI_MakeChamfer`

## 0.5.1.1

## 0.5.1.0

### Added

- Add `OpenCascade.BOPAlgo.Builder`
- Add `OpenCascade.BOPAlgo.BOP`
- Add `OpenCascade.BOPAlgo.Operation`

## 0.5.0.1

## 0.5.0.0

### Added

- Add `OpenCascade.BRepFilletAPI.MakeFillet` methods `reset`, `nbFaultyContours`, `faultyContour`, `nbEdges`, `edge` and `remove`
- Add `OpenCascade.BRepBuilderAPI.MakeShape` method `build`
- Add `OpenCascade.BRepOffsetAPI.MakePipe` constructor `fromWireShapeTrihedronModeAndForceC1`
- Add `OpenCascade.GeomFill.Trihedron`
- Add `OpenCascade.BRepAdaptor.Curve`
- Add `OpenCascade.GeomAbs.CurveType`
- Add `OpenCascade.GeomAbs.Shape`
- Add `OpenCascade.GeomConvert.BSplineCurveToBezierCurve`
- Add `OpenCascade.GeomConvert.ApproxCurve`
- Add `OpenCascade.Geom.BSplineCurve`
- Add `OpenCascade.Geom.BezierCurve` methods `pole`, `nbPoles` and `isRational`
- Add `OpenCascade.GeomAdaptor.Curve`
- Add `OpenCascade.ShapeConstruct.Curve`
- Add `OpenCascade.HLRAlgo.Projector`
- Add `OpenCascade.HLRBRep.Algo`
- Add `OpenCascade.HLRBRep.HLRToShape`
- Add `OpenCascade.HLRBRep.TypeOfResultingEdge`
- Add `OpenCascade.BRepLib` method `buildCurve3d`
- Add `OpenCascade.Geom.Curve` methods `firstParameter` and `lastParameter`

## 0.4.0.0

### Added

- Add `OpenCascade.BRepBuilderAPI.MakeVertex`
- Add `OpenCascade.BRepBuilderAPI.MakeWire.vertex`
- Add `OpenCascade.BRepOffsetAPI.ThruSections`
- Add `OpenCascade.BRep.Tool.pnt`
- Add `OpenCascade.Geom.Curve` methods `reversedParameter` and `reversed`
- Add `OpenCascade.ShapeExtend.WireData`
- Add `Show` instance to `OpenCascade.BRepBuilderAPI.WireError`

## 0.3.0.1

## 0.3.0.0

### Changed

- The project now depends on (and supports) OpenCascade `7.8.0` or later

### Added 

- added `OpenCascade.TopTools.ShapeMapHasher` (this replaces `hashCode` in `OpenCascade.TopoDS.Shape`)

### Removed

- removed `OpenCascade.TopoDS.Shape.hashCode` 

## 0.2.2.1

## 0.2.2.0

### Fixed

- Fix `OpenCascade.GProps.fromSystemLocation`

### Added 

- Added OpenCascade.StlAPI.Reader
- Added OpenCascade.ShapeFix.Solid
- Added OpenCascade.ShapeExtend.Status
- Added OpenCascade.STEPControl.Reader
- Added OpenCascade.XSControl.Reader
- Added OpenCascade.RWGltf.CafReader
- Added OpenCascade.RWMesh.CafReader
- Added OpenCascade.TopoDS.Builder
- Added OpenCascade.BRepBuilderAPI.Copy
- Added OpenCascade.BRepBuilderAPI.Sewing
- Added OpenCascade.Poly.Triangulation
- Added OpenCascade.Poly.Triangle
- Added OpenCascade.BRep.Tool.triangulation
- Added OpenCascade.BRepBuilderAPI.MakePolygon
- Added OpenCascade.RWObj.CafWriter
- Added OpenCascade.RWObj.CafReader
- Added OpenCascade.BRepLib (orientClosedSolid)

## 0.2.1.0

### Added

- Added OpenCascade.Message.ProgressRange
- Added OpenCascade.RWGltf.CafWriter
- Added OpenCascade.RWGltf.Types
- Added OpenCascade.TColStd.IndexedDataMapOfStringString
- Added OpenCascade.TDocStd.Document
- Added OpenCascade.XCAFDoc.DocumentTool
- Added OpenCascade.XCAFDoc.ShapeTool

## 0.2.0.0

### Added

- Added OpenCascade.BRepGProp
- Added OpenCascade.GProp.GProps
- Added OpenCascade.BRepBndLib 
- Added OpenCascade.Bnd.Box
- Added OpenCascade.Bnd.OBB
- Added OpenCascade.GP.XYZ (just the barebones Constructor and Getter/Setter methods)
- Added `setDisplacement` to OpenCascade.GP.Trsf

## 0.1.2.2 - 2024-01-09 

## 0.1.2.1 - 2024-01-09 

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
