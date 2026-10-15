#include <ShapeUpgrade_UnifySameDomain.hxx>
#include <TopoDS_Shape.hxx>
#include "hs_Exception.h"
#include "hs_ShapeUpgrade_UnifySameDomain.h"

ShapeUpgrade_UnifySameDomain * hs_new_ShapeUpgrade_UnifySameDomain_fromShape(
        TopoDS_Shape * shape, bool unifyEdges, bool unifyFaces, bool concatBSplines,
        HSExceptionType* exType, void ** exPtr
){
    return hs_handleEx(exType, exPtr, [shape, unifyEdges, unifyFaces, concatBSplines]{
        return new ShapeUpgrade_UnifySameDomain(*shape, unifyEdges, unifyFaces, concatBSplines);
    });
}

void hs_delete_ShapeUpgrade_UnifySameDomain(ShapeUpgrade_UnifySameDomain * unify){
    delete unify;
}

void hs_ShapeUpgrade_UnifySameDomain_setSafeInputMode(ShapeUpgrade_UnifySameDomain * unify, bool value){
    unify->SetSafeInputMode(value);
}

void hs_ShapeUpgrade_UnifySameDomain_allowInternalEdges(ShapeUpgrade_UnifySameDomain * unify, bool value){
    unify->AllowInternalEdges(value);
}

void hs_ShapeUpgrade_UnifySameDomain_build(
        ShapeUpgrade_UnifySameDomain * unify,
        HSExceptionType* exType, void ** exPtr
){
    hs_handleExVoid(exType, exPtr, [unify]{
        unify->Build();
    });
}

TopoDS_Shape * hs_ShapeUpgrade_UnifySameDomain_shape(ShapeUpgrade_UnifySameDomain * unify){
    return new TopoDS_Shape(unify->Shape());
}
