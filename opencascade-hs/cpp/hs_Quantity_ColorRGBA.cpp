#include <Quantity_ColorRGBA.hxx>
#include "hs_Quantity_ColorRGBA.h"

Quantity_ColorRGBA * hs_new_Quantity_ColorRGBA(Quantity_Color * color, double alpha){
    return new Quantity_ColorRGBA(*color, (float) alpha);
}

void hs_delete_Quantity_ColorRGBA(Quantity_ColorRGBA * color){
    delete color;
}
