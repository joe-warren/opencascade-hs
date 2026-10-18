#include <gp_Lin.hxx>
#include "hs_gp_Lin.h"

gp_Lin * hs_new_gp_Lin(gp_Pnt * origin, gp_Dir * direction){
    return new gp_Lin(*origin, *direction);
}

void hs_delete_gp_Lin(gp_Lin* lin){
    delete lin;
}

gp_Pnt * hs_gp_Lin_Location(gp_Lin* lin){
    return new gp_Pnt(lin->Location());
}

gp_Dir * hs_gp_Lin_Direction(gp_Lin* lin){
    return new gp_Dir(lin->Direction());
}
