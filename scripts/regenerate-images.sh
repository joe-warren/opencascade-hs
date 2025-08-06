#!/bin/sh
stack build
stack exec waterfall-cad-examples --  --csg --dark-mode-svg images/csg.svg --height 200
stack exec waterfall-cad-examples --  --gear --dark-mode-svg images/gear.svg
stack exec waterfall-cad-examples --  --revolution --dark-mode-svg images/revolution.svg --height 200
stack exec waterfall-cad-examples --  --sweep --dark-mode-svg images/sweep.svg --height 200
stack exec waterfall-cad-examples --  --offset --dark-mode-svg images/offset.svg
stack exec waterfall-cad-examples --  --text --content "Waterfall CAD" --dark-mode-svg images/text.svg --font ~/.fonts/GothamRounded-Bold.otf
stack exec waterfall-cad-examples --  --bound --dark-mode-svg images/bounding-boxes.svg --height 200
stack exec waterfall-cad-examples --  --loft --dark-mode-svg images/loft.svg

stack exec waterfall-cad-examples --  --csg --glb waterfall-cad-examples/models/csg.glb
stack exec waterfall-cad-examples --  --gear --glb waterfall-cad-examples/models/gear.glb
stack exec waterfall-cad-examples --  --revolution --glb waterfall-cad-examples/models/revolution.glb
stack exec waterfall-cad-examples --  --sweep --glb waterfall-cad-examples/models/sweep.glb
stack exec waterfall-cad-examples --  --offset --glb waterfall-cad-examples/models/offset.glb
stack exec waterfall-cad-examples --  --text --content "Waterfall CAD" --glb waterfall-cad-examples/models/text.glb --font ~/.fonts/GothamRounded-Bold.otf
stack exec waterfall-cad-examples --  --bound --glb waterfall-cad-examples/models/bounding-boxes.glb
stack exec waterfall-cad-examples --  --loft --glb waterfall-cad-examples/models/loft.glb