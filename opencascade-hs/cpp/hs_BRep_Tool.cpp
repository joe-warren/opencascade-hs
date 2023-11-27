#include <BRep_Tool.hxx>
#include <Standard_Handle.hxx>
#include "hs_BRep_Tool.h"

Handle(Geom_Curve) * hs_BRep_Tool_curve(TopoDS_Edge * edge){
    double s, e;
    return new Handle(Geom_Curve)(BRep_Tool::Curve(*edge, s, e));
}

double hs_BRep_Tool_curveParamFirst(TopoDS_Edge * edge){
    double s, e;
    BRep_Tool::Curve(*edge, s, e);
    return s;
}

double hs_BRep_Tool_curveParamLast(TopoDS_Edge * edge){
    double s, e;
    BRep_Tool::Curve(*edge, s, e);
    return e;
} 


