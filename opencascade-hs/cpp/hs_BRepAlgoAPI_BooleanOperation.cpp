#include <BRepAlgoAPI_BooleanOperation.hxx>
#include "hs_Exception.h"
#include "hs_BRepAlgoAPI_BooleanOperation.h"

void hs_BRepAlgoAPI_BooleanOperation_setTools(
    BRepAlgoAPI_BooleanOperation * builder, 
    LIST(TopoDS_Shape) * tools, 
    HSExceptionType* exType,
    void** exPtr
){
    return hs_handleExVoid(
        exType,
        exPtr,
        [builder ,tools]{
        builder->SetTools(*tools);
    });
}