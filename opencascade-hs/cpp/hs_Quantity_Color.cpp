#include <Quantity_Color.hxx>
#include "hs_Exception.h"
#include "hs_Quantity_Color.h"

Quantity_Color * hs_new_Quantity_Color(
        double c1, double c2, double c3, Quantity_TypeOfColor theType,
        HSExceptionType* exType, void ** exPtr
    ){
    return hs_handleEx(exType, exPtr, [c1, c2, c3, theType]{
        return new Quantity_Color(c1, c2, c3, theType);
    });
}

void hs_delete_Quantity_Color(Quantity_Color * color){
    delete color;
}
