# open-cascade

This is my attempt to write a third party Haskell wrapper to [OpenCASCADE](https://www.opencascade.com/)([wiki](https://en.wikipedia.org/wiki/Open_Cascade_Technology)), which is the underlying framework behind [FreeCAD](https://www.freecad.org/).

It's _very early days_, so this is not useable at all yet.

My plan is to focus on wrapping the modeling functionality (BRep/Boolean Ops/Curves/etc) before even contemplating any work on the visualization components. This is because my personal usecase for this library is as a framework for declarative CAD. 
