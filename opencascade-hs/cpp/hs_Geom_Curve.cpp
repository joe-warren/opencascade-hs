#include <Geom_Curve.hxx>
#include <GeomAbs_CurveType.hxx>
#include <gp_Pnt.hxx>
#include <gp_Vec.hxx>
#include "hs_Exception.h"
#include "hs_Geom_Curve.h"

void hs_delete_Handle_Geom_Curve(Handle(Geom_Curve) * handle){
    delete handle;
}

gp_Pnt * hs_Geom_Curve_value(
        Handle(Geom_Curve) * curve, double u,
        HSExceptionType* exType, void ** exPtr
){
    return hs_handleEx(
        exType,
        exPtr,
        [curve, u]{
            return new gp_Pnt((*curve)->Value(u));
        }
    );
}

double hs_Geom_Curve_firstParameter(Handle(Geom_Curve) * curve){
    return (*curve)->FirstParameter();
}

double hs_Geom_Curve_lastParameter(Handle(Geom_Curve) * curve){
    return (*curve)->LastParameter();
}

gp_Vec * hs_Geom_Curve_dn(
        Handle (Geom_Curve) * curve, double u, int n,
        HSExceptionType* exType, void ** exPtr
){
    return hs_handleEx(
        exType,
        exPtr,
        [curve, u, n]{
            return new gp_Vec((*curve)->DN(u, n));
        }
    );
}

double hs_Geom_Curve_reversedParameter(
        Handle (Geom_Curve) * curve, double parameter,
        HSExceptionType* exType, void ** exPtr
){
    return hs_handleExWithDefault(
        exType,
        exPtr,
        [curve, parameter]{
            return (*curve)->ReversedParameter(parameter);
        },
        0.0
    );
}

Handle (Geom_Curve) * hs_Geom_Curve_reversed(
        Handle (Geom_Curve) * curve,
        HSExceptionType* exType, void ** exPtr
){
    return hs_handleEx(
        exType,
        exPtr,
        [curve]{
            return new opencascade::handle<Geom_Curve>((*curve)->Reversed());
        }
    );
}
