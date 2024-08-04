#include <ShapeExtend_WireData.hxx>
#include "hs_ShapeExtend_WireData.h"

#include <TopoDS_Wire.hxx>

ShapeExtend_WireData * hs_new_ShapeExtend_WireData_fromWireChainedAndManifold(TopoDS_Wire* wire, bool chained, bool manifoldMode){
    return new ShapeExtend_WireData(*wire, chained, manifoldMode);
}

void hs_delete_ShapeExtend_WireData(ShapeExtend_WireData * wireData){
    delete wireData;
}

void hs_ShapeExtend_WireData_reverse(ShapeExtend_WireData * wireData){
    wireData->Reverse();
}

TopoDS_Wire * hs_ShapeExtend_WireData_wire(ShapeExtend_WireData * wireData){
    return new TopoDS_Wire(wireData->Wire());
}


TopoDS_Wire * hs_ShapeExtend_WireData_wireAPIMake(ShapeExtend_WireData * wireData){
    return new TopoDS_Wire(wireData->WireAPIMake());
}