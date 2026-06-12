#include <TopTools_ShapeMapHasher.hxx>
#include "hs_Exception.h"
#include "hs_TopTools_ShapeMapHasher.h"

int hs_TopTools_ShapeMapHasher_hash(TopoDS_Shape * shape, HSExceptionType* exType, void ** exPtr){
    return hs_handleExWithDefault(
        exType, 
        exPtr, 
        [shape]{
            auto hasher = TopTools_ShapeMapHasher();
            return hasher(*shape);
        }, 
        0
    );
}

bool hs_TopTools_ShapeMapHasher_isEqual(TopoDS_Shape * shapeA, TopoDS_Shape * shapeB, HSExceptionType* exType, void ** exPtr){
    return hs_handleExWithDefault(
        exType, 
        exPtr, 
        [shapeA, shapeB]{
            auto hasher = TopTools_ShapeMapHasher();
            return hasher(*shapeA, *shapeB);
        }, 
        false
    );
}