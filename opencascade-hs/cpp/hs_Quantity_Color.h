#ifndef HS_QUANTITY_COLOR_H
#define HS_QUANTITY_COLOR_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

Quantity_Color * hs_new_Quantity_Color(
    double c1, double c2, double c3, Quantity_TypeOfColor theType,
    HSExceptionType* exType, void ** exPtr
);

void hs_delete_Quantity_Color(Quantity_Color * color);

#ifdef __cplusplus
}
#endif

#endif // HS_QUANTITY_COLOR_H
