# OpenCASCADE-hs

This is my attempt to write a third party Haskell wrapper to [OpenCASCADE](https://www.opencascade.com/)([wiki](https://en.wikipedia.org/wiki/Open_Cascade_Technology)), which is the underlying framework behind [FreeCAD](https://www.freecad.org/).

It's _very early days_, so the API is subject to change dramatically.

My plan is to focus on wrapping the modeling functionality (BRep/Boolean Ops/Curves/etc) before even contemplating any work on the visualization components. This is because my personal usecase for this library is as a framework for declarative CAD. 

# Waterfall CAD

In parallel, I'm developing a Declarative CAD/Solid Modeling library, called Waterfall-CAD.
This uses opencascade-hs as the kernel, but provides a "more functional" API over it.

Like `opencascade-hs` the API is subject to change dramatically.

# Building

I've only been testing this on Debian, it probably won't build on MacOS or Windows. 

The underlying OpenCASCADE library is portable though, so I'd like to fix that eventually.

I've been building this project with [`stack`](https://docs.haskellstack.org/en/stable/).

As this library depends on OpenCASCADE. 
I've been developing this on Debian with the following packages installed:

* `libocct-data-exchange-7.6` `libocct-data-exchange-dev` `libocct-draw-7.6` `libocct-draw-dev`, `libocct-foundation-7.6`, `libocct-foundation-dev`, `libocct-modeling-algorithms-7.6`, `libocct-modeling-algorithms-dev`, `libocct-modeling-data-7.6`, `libocct-modeling-data-dev`, `libocct-ocaf-7.6`, `libocct-ocaf-dev`, `libocct-visualization-7.6`, `libocct-visualization-dev`, `occt-misc`

I think there should be _some_ flexibility as to the exact version of libocct required, and which occt packages are necessary, but I'm not at a stage where I want to nail that down yet.


# Licensing

Because OpenCASCADE is licensed under the LGPL version 2.1, I'm also using that library for this project.

# Examples 

[![](images/csg.png)](waterfall-cad-examples/src/CsgExample.hs)

[![](images/gear.png)](waterfall-cad-examples/src/GearExample.hs)

[![](images/revolution.png)](waterfall-cad-examples/src/RevolutionExample.hs)

[![](images/sweep.png)](waterfall-cad-examples/src/SweepExample.hs)