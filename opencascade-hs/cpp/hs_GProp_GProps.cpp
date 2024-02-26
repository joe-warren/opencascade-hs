#include <GProp_GProps.hxx>
#include "hs_GProp_GProps.h"

GProp_GProps * hs_new_GProp_GProps(){
    return new GProp_GProps();
}

GProp_GProps * hs_new_GProp_GProps_fromSystemLocation(gp_Pnt * pnt){
    return new GProp_GProps(*pnt);
}

void  hs_delete_GProp_GProps(GProp_GProps * props){
    delete props;
}

double hs_GProp_GProps_mass(GProp_GProps * props){
    return props->Mass();
}

gp_Pnt * hs_GProp_GProps_centreOfMass(GProp_GProps * props){
    return new gp_Pnt(props->CentreOfMass());
}

double hs_GProp_GProps_momentOfInertia(GProp_GProps * props, gp_Ax1 * ax){
    return props->MomentOfInertia(*ax);
}