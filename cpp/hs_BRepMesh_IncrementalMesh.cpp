#include <BRepMesh_IncrementalMesh.hxx>
#include "hs_BRepMesh_IncrementalMesh.h"


BRepMesh_IncrementalMesh * hs_BRepMesh_IncrementalMesh_fromShapeAndLinDeflection(TopoDS_Shape *shape, double theLinDeflection){
    return new BRepMesh_IncrementalMesh(*shape, theLinDeflection);
}

void hs_delete_BRepMesh_IncrementalMesh(BRepMesh_IncrementalMesh * mesh){
    delete mesh;
}

void hs_BRepMesh_IncrementalMesh_Perform(BRepMesh_IncrementalMesh * mesh){
    mesh->Perform();
}