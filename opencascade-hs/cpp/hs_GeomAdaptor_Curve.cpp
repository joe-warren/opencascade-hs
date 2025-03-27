#include <GeomAdaptor_Curve.hxx>
#include "hs_GeomAdaptor_Curve.h"

GeomAdaptor_Curve * hs_new_GeomAdaptor_Curve_fromHandle(Handle(Geom_Curve) *curve){
    return new GeomAdaptor_Curve(*curve);
}

void hs_delete_GeomAdaptor_Curve(GeomAdaptor_Curve * adaptor){
    delete adaptor;
}

double hs_GeomAdaptor_Curve_firstParameter(GeomAdaptor_Curve * adaptor){
    return adaptor->FirstParameter();
}

double hs_GeomAdaptor_Curve_lastParameter(GeomAdaptor_Curve * adaptor){
    return adaptor->LastParameter();
}

Handle(Geom_Curve)* hs_GeomAdaptor_Curve_curve(GeomAdaptor_Curve * adaptor){
    return new opencascade::handle(adaptor->Curve());
}