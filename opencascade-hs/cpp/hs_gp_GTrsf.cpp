#include <gp_GTrsf.hxx>
#include "hs_Exception.h"
#include "hs_gp_GTrsf.h"

gp_GTrsf * hs_new_gp_GTrsf(){
    return new gp_GTrsf();
}

void hs_delete_gp_GTrsf(gp_GTrsf * t){
    delete t;
}

void hs_gp_GTrsf_setValue(gp_GTrsf * trsf, int row, int col, double value, HSExceptionType* exType, void ** exPtr){
    hs_handleExVoid(exType, exPtr, [trsf, row, col, value]{
        trsf->SetValue(row, col, value);
    });
}


void hs_gp_GTrsf_setForm(gp_GTrsf * trsf, HSExceptionType* exType, void ** exPtr){
    hs_handleExVoid(exType, exPtr, [trsf]{
        trsf->SetForm();
    });
}