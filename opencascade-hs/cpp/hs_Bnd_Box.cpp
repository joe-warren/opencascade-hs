#include <Bnd_Box.hxx>
#include "hs_Exception.h"
#include "hs_Bnd_Box.h"

Bnd_Box * hs_new_Bnd_Box(){
    return new Bnd_Box();
}

void hs_delete_Bnd_Box(Bnd_Box * box){
    delete box;
}

gp_Pnt * hs_Bnd_Box_cornerMin(
        Bnd_Box * box,
        HSExceptionType* exType, void ** exPtr
){
    return hs_handleEx(exType, exPtr, [box]{
        return new gp_Pnt(box->CornerMin());
    });
}

gp_Pnt * hs_Bnd_Box_cornerMax(
        Bnd_Box * box,
        HSExceptionType* exType, void ** exPtr
){
    return hs_handleEx(exType, exPtr, [box]{
        return new gp_Pnt(box->CornerMax());
    });
}