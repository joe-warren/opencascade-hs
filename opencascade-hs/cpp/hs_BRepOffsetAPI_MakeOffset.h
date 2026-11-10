#ifndef HS_BREPOFFSETAPI_MAKEOFFSET_H
#define HS_BREPOFFSETAPI_MAKEOFFSET_H

#include "hs_types.h"
#include "hs_Exception.h"

#ifdef __cplusplus
extern "C" {
#endif

BRepOffsetAPI_MakeOffset * hs_new_BRepOffsetAPI_MakeOffset();

void hs_delete_BRepOffsetAPI_MakeOffset(BRepOffsetAPI_MakeOffset * builder);

BRepOffsetAPI_MakeOffset * hs_new_BRepOffsetAPI_MakeOffset_fromFace
    ( TopoDS_Face* face
    , GeomAbs_JoinType join
    , bool isOpenResult
    , HSExceptionType* exType
    , void** exPtr
    );

void hs_BRepOffsetAPI_MakeOffset_Init
    (BRepOffsetAPI_MakeOffset *builder
    , TopoDS_Face* face
    , GeomAbs_JoinType join
    , bool isOpenResult
    , HSExceptionType* exType
    , void** exPtr
    );

void hs_BRepOffsetAPI_MakeOffset_AddWire
    ( BRepOffsetAPI_MakeOffset * builder
    , TopoDS_Wire* wire
    , HSExceptionType* exType
    , void** exPtr
    );

void hs_BRepOffsetAPI_MakeOffset_Perform
    ( BRepOffsetAPI_MakeOffset * builder
    , double offset
    , double alt
    , HSExceptionType* exType
    , void** exPtr
    );
        

#ifdef __cplusplus
}
#endif

#endif // HS_BREPOFFSETAPI_MAKEOFFSET_H
