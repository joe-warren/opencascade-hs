#!/bin/sh
stack build
stack exec waterfall-cad-examples --  --csg --dark-mode-svg images/csg.svg --height 200
stack exec waterfall-cad-examples --  --gear --dark-mode-svg images/gear.svg
stack exec waterfall-cad-examples --  --revolution --dark-mode-svg images/revolution.svg --height 200
stack exec waterfall-cad-examples --  --sweep --dark-mode-svg images/sweep.svg --height 200
stack exec waterfall-cad-examples --  --offset --dark-mode-svg images/offset.svg
stack exec waterfall-cad-examples --  --text --content "Waterfall CAD" --dark-mode-svg images/text.svg --font images/fonts/varela/VarelaRound-Regular.ttf 
stack exec waterfall-cad-examples --  --bound --dark-mode-svg images/bounding-boxes.svg --height 200
stack exec waterfall-cad-examples --  --loft --dark-mode-svg images/loft.svg
stack exec waterfall-cad-examples --  --2d-booleans --dark-mode-svg images/2d-booleans.svg --height 200
stack exec waterfall-cad-examples --  --platonic-solids --dark-mode-svg images/platonic.svg --height 200

stack exec waterfall-cad-examples --  --csg --glb images/models/csg.glb
stack exec waterfall-cad-examples --  --gear --glb images/models/gear.glb
stack exec waterfall-cad-examples --  --revolution --glb images/models/revolution.glb
stack exec waterfall-cad-examples --  --sweep --glb images/models/sweep.glb --resolution 0.01
stack exec waterfall-cad-examples --  --offset --glb images/models/offset.glb
stack exec waterfall-cad-examples --  --text --content "Waterfall CAD" --glb images/models/text.glb --font images/fonts/varela/VarelaRound-Regular.ttf --resolution 0.01
stack exec waterfall-cad-examples --  --bound --glb images/models/bounding-boxes.glb
stack exec waterfall-cad-examples --  --loft --glb images/models/loft.glb
stack exec waterfall-cad-examples --  --fillet --glb images/models/fillet.glb
stack exec waterfall-cad-examples --  --prism --glb images/models/prism.glb
stack exec waterfall-cad-examples --  --2d-booleans --glb images/models/2d-booleans.glb
stack exec waterfall-cad-examples --  --platonic-solids --glb images/models/platonic.glb