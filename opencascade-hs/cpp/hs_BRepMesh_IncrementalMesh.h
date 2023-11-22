#ifndef HS_BREPMESH_INCREMENTALMESH_H
#define HS_BREPMESH_INCREMENTALMESH_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

BRepMesh_IncrementalMesh * hs_BRepMesh_IncrementalMesh_fromShapeAndLinDeflection(TopoDS_Shape *shape, double theLinDeflection);

void hs_delete_BRepMesh_IncrementalMesh(BRepMesh_IncrementalMesh * mesh);

void hs_BRepMesh_IncrementalMesh_Perform(BRepMesh_IncrementalMesh * mesh);

#ifdef __cplusplus
}
#endif

#endif // HS_BREPMESH_INCREMENTALMESH_H
