#include <BRepOffsetAPI_MakeOffset.hxx>
#include "hs_BRepOffsetAPI_MakeOffset.h"
#include "hs_Exception.h"

BRepOffsetAPI_MakeOffset * hs_new_BRepOffsetAPI_MakeOffset(){
    return new BRepOffsetAPI_MakeOffset();
}

void hs_delete_BRepOffsetAPI_MakeOffset(BRepOffsetAPI_MakeOffset * builder){
    delete builder;
}

BRepOffsetAPI_MakeOffset * hs_new_BRepOffsetAPI_MakeOffset_fromFace
    ( TopoDS_Face* face
    , GeomAbs_JoinType join
    , bool isOpenResult
    , HSExceptionType* exType
    , void** exPtr
    ){
    return hs_handleEx(
        exType,
        exPtr,
        [face, join, isOpenResult]{
            return new BRepOffsetAPI_MakeOffset(*face, join, isOpenResult);
        }
    );
}


void hs_BRepOffsetAPI_MakeOffset_Init
    (BRepOffsetAPI_MakeOffset *builder
    , TopoDS_Face* face
    , GeomAbs_JoinType join
    , bool isOpenResult
    , HSExceptionType* exType
    , void** exPtr
    ){
    hs_handleExVoid(
        exType,
        exPtr,
        [builder, face, join, isOpenResult]{
            builder->Init(*face, join, isOpenResult);
        }
    );
}

void hs_BRepOffsetAPI_MakeOffset_AddWire
    ( BRepOffsetAPI_MakeOffset * builder
    , TopoDS_Wire* wire
    , HSExceptionType* exType
    , void** exPtr
    ) {
    hs_handleExVoid(
        exType,
        exPtr,
        [builder, wire]{
            builder->AddWire(*wire);
        }
    );
}

void hs_BRepOffsetAPI_MakeOffset_Perform
    ( BRepOffsetAPI_MakeOffset * builder
    , double offset
    , double alt
    , HSExceptionType* exType
    , void** exPtr
    ) {
    hs_handleExVoid(
        exType,
        exPtr,
        [builder, offset, alt]{
            builder->Perform(offset, alt);
        }
    );
}