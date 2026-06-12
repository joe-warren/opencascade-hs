#include <BRepBuilderAPI_MakeVertex.hxx>
#include "hs_Exception.h"
#include "hs_BRepBuilderAPI_MakeVertex.h"
#include <TopoDS_Vertex.hxx>

BRepBuilderAPI_MakeVertex * hs_new_BRepBuilderAPI_MakeVertex_fromPnt(
        gp_Pnt* pnt,
        HSExceptionType* exType,
        void** exPtr
){
    return hs_handleEx(
        exType,
        exPtr,
        [pnt]{
        return new BRepBuilderAPI_MakeVertex(*pnt);
    });
}

void hs_delete_BRepBuilderAPI_MakeVertex(BRepBuilderAPI_MakeVertex* builder){
    delete builder;
}

TopoDS_Vertex * hs_BRepBuilderAPI_MakeVertex_vertex(
        BRepBuilderAPI_MakeVertex * builder,
        HSExceptionType* exType,
        void** exPtr
){
    return hs_handleEx(
        exType,
        exPtr,
        [builder]{
        return new TopoDS_Vertex(builder->Vertex());
    });
}