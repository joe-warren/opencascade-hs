#ifndef HS_SHAPEEXTEND_WIREDATA_H
#define HS_SHAPEEXTEND_WIREDATA_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

ShapeExtend_WireData * hs_new_ShapeExtend_WireData_fromWireChainedAndManifold(TopoDS_Wire* wire, bool chained, bool manifoldMode);

void hs_delete_ShapeExtend_WireData(ShapeExtend_WireData * wireData);

void hs_ShapeExtend_WireData_reverse(ShapeExtend_WireData * wireData);

TopoDS_Wire * hs_ShapeExtend_WireData_wire(ShapeExtend_WireData * wireData);

TopoDS_Wire * hs_ShapeExtend_WireData_wireAPIMake(ShapeExtend_WireData * wireData);

#ifdef __cplusplus
}
#endif

#endif // HS_SHAPEEXTEND_WIREDATA_H