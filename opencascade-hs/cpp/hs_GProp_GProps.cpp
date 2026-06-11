#include <GProp_GProps.hxx>
#include "hs_Exception.h"
#include "hs_GProp_GProps.h"

GProp_GProps * hs_new_GProp_GProps(
        HSExceptionType* exType, void ** exPtr
){
    return hs_handleEx(exType, exPtr, []{
        return new GProp_GProps();
    });
}

GProp_GProps * hs_new_GProp_GProps_fromSystemLocation(
        gp_Pnt * pnt,
        HSExceptionType* exType, void ** exPtr
){
    return hs_handleEx(exType, exPtr, [pnt]{
        return new GProp_GProps(*pnt);
    });
}

void  hs_delete_GProp_GProps(GProp_GProps * props){
    delete props;
}

double hs_GProp_GProps_mass(
        GProp_GProps * props,
        HSExceptionType* exType, void ** exPtr
){
    return hs_handleExWithDefault(exType, exPtr, [props]{
        return props->Mass();
    }, 0.0);
}

gp_Pnt * hs_GProp_GProps_centreOfMass(
        GProp_GProps * props,
        HSExceptionType* exType, void ** exPtr
){
    return hs_handleEx(exType, exPtr, [props]{
        return new gp_Pnt(props->CentreOfMass());
    });
}

double hs_GProp_GProps_momentOfInertia(
        GProp_GProps * props, gp_Ax1 * ax,
        HSExceptionType* exType, void ** exPtr
){
    return hs_handleExWithDefault(exType, exPtr, [props, ax]{
        return props->MomentOfInertia(*ax);
    }, 0.0);
}