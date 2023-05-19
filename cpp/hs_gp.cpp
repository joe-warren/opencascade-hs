#include <gp.hxx>
#include <gp_Pnt.hxx>
#include <gp_Pnt2d.hxx>
#include <gp_Dir.hxx>
#include <gp_Dir2d.hxx>
#include <gp_Ax1.hxx>
#include <gp_Ax2.hxx>
#include <gp_Ax2d.hxx>
#include "hs_gp.h"

gp_Pnt * hs_gp_Origin(){ 
    return new gp_Pnt(gp::Origin());
}

gp_Dir * hs_gp_DX(){ 
    return new gp_Dir(gp::DX());
}

gp_Dir * hs_gp_DY(){ 
    return new gp_Dir(gp::DY());
}

gp_Dir * hs_gp_DZ(){ 
    return new gp_Dir(gp::DZ());
}

gp_Ax1 * hs_gp_OX(){ 
    return new gp_Ax1(gp::OX());
}

gp_Ax1 * hs_gp_OY(){ 
    return new gp_Ax1(gp::OY());
}

gp_Ax1 * hs_gp_OZ(){ 
    return new gp_Ax1(gp::OZ());
}

gp_Ax2 * hs_gp_XOY(){ 
    return new gp_Ax2(gp::XOY());
}

gp_Ax2 * hs_gp_YOZ(){ 
    return new gp_Ax2(gp::YOZ());
}

gp_Ax2 * hs_gp_ZOX(){ 
    return new gp_Ax2(gp::ZOX());
}

gp_Pnt2d * hs_gp_Origin2d(){ 
    return new gp_Pnt2d(gp::Origin2d());
}

gp_Dir2d * hs_gp_DX2d(){ 
    return new gp_Dir2d(gp::DX2d());
}

gp_Dir2d * hs_gp_DY2d(){ 
    return new gp_Dir2d(gp::DY2d());
}

gp_Ax2d * hs_gp_OX2d(){ 
    return new gp_Ax2d(gp::OX2d());
}

gp_Ax2d * hs_gp_OY2d(){ 
    return new gp_Ax2d(gp::OY2d());
}
