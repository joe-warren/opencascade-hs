#include <BRep_Tool.hxx>
#include <Standard_Handle.hxx>
#include "hs_BRep_Tool.h"

Handle(Geom_Curve) * hs_BRep_Tool_curve(TopoDS_Edge * edge, double first, double last){
    return new Handle(Geom_Curve)(BRep_Tool::Curve(*edge, first, last));
} 

