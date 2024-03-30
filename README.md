This repository contains two libraries, "OpenCascade-hs" and "Waterfall CAD".

## ![OpenCASCADE-hs](images/logo/opencascade-hs-logo-name.svg)

A third party Haskell wrapper to [Open CASCADE](https://dev.opencascade.org) ([wiki](https://en.wikipedia.org/wiki/Open_Cascade_Technology)), which is the underlying framework behind [FreeCAD](https://www.freecad.org/).

Wrapping the Open Cascade API has been done on an incremental basis, and only a partially subset has been extracted, this largely consists of the modeling functionality (BRep/Boolean Ops/Curves/etc), and does not include the visualization components.

## ![Waterfall CAD](images/logo/waterfall-cad-logo-name.svg)

Waterfall CAD is a declarative CAD/Solid Modeling library.

This uses [opencascade-hs](https://hackage.haskell.org/package/opencascade-hs) as the kernel, but provides a "more functional" API over it.

## Installing Dependencies

I've only been testing this on Debian and MacOS.

### Linux

As this library depends on OpenCASCADE, I've been developing on Debian with the following packages installed:

* `libocct-data-exchange-7.6` `libocct-data-exchange-dev` `libocct-draw-7.6` `libocct-draw-dev`, `libocct-foundation-7.6`, `libocct-foundation-dev`, `libocct-modeling-algorithms-7.6`, `libocct-modeling-algorithms-dev`, `libocct-modeling-data-7.6`, `libocct-modeling-data-dev`, `libocct-ocaf-7.6`, `libocct-ocaf-dev`, `libocct-visualization-7.6`, `libocct-visualization-dev`, `occt-misc`

I think there should be _some_ flexibility as to the exact version of libocct required, and which occt packages are necessary.

### MacOS

On MacOS, you should be able to install [OpenCASCADE](https://formulae.brew.sh/formula/opencascade) via [homebrew](https://brew.sh/):

```
brew install opencascade
```

You'll also need to configure the path to the OpenCASCADE header files, either via `extra-include-dirs`, or `CPATH`:

```
export CPATH=$CPATH:/usr/local/Cellar/opencascade/7.*/include/opencascade/
```

## Licensing

Because OpenCASCADE is licensed under the LGPL version 2.1, I'm also using that library for this project.

## Get in Touch

If you're having any problems using either of these libraries, please feel free to [open an issue](https://github.com/joe-warren/opencascade-hs/issues).

If you've found the project useful, or interesting, or if you've built anything with it, I'd love to hear from you: there's a [Waterfall-CAD Discord server](https://discord.gg/aHfA4XKpyA).

## Examples 

[![](images/csg.png)](waterfall-cad-examples/src/CsgExample.hs)

[![](images/gear.png)](waterfall-cad-examples/src/GearExample.hs)

[![](images/revolution.png)](waterfall-cad-examples/src/RevolutionExample.hs)

[![](images/sweep.png)](waterfall-cad-examples/src/SweepExample.hs)

[![](images/offset.png)](waterfall-cad-examples/src/OffsetExample.hs)

[![](images/text.png)](waterfall-cad-examples/src/TextExample.hs)

[![](images/bounding_boxes.png)](waterfall-cad-examples/src/BoundingBoxExample.hs)
