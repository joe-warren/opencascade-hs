# Changelog for `waterfall-cad-svg`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

## 0.7.0.0

### Added

- Added paths for painted regions of a `Diagram`, with class `fill`
- Added `Waterfall.SVG.ToSVG.LineStyle`, `Waterfall.SVG.ToSVG.lineStyleCss` and `Waterfall.SVG.ToSVG.diagramToSvgWithLineStyle`
- Added `Generic` and [`NFData`](https://hackage-content.haskell.org/package/deepseq/docs/Control-DeepSeq.html#t:NFData) instances for the following
  - `Waterfall.SVG.ToSVG.LineStyle`
  - `Waterfall.SVG.FromSVG.SVGError`
  - `Waterfall.SVG.FromSVG.SVGErrorKind`

### Changed

- Edge paths carry only the class `edge`: `visible`/`hidden` and `sharp`/`outline` are now classes on the enclosing groups
- The default hidden-line colour and opacity changed from light blue at full opacity to black at 0.4 opacity

### Fixed

- `path2DToPathCommands` generated extraneous `MoveTo` commands, and incorrectly omitted the `MoveTo` command before the first segment of loops with two edges

## 0.6.3.2

## 0.6.3.1

## 0.6.3.0

- the SVG CSS now declares the Path fill opacity as 0

## 0.6.2.1

## 0.6.2.0

## 0.6.1.0

## 0.6.0.1

## 0.6.0.0

## 0.5.1.1

## 0.5.1.0

## 0.5.0.1

### Changed

- Svg Path segments that originate from the same edge should no longer contain breaks

## 0.5.0.0

- Initial Release
