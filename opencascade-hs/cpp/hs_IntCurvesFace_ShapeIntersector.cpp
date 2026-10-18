#include <IntCurvesFace_ShapeIntersector.hxx>
#include <TopoDS_Shape.hxx>
#include <TopoDS_Face.hxx>
#include <gp_Lin.hxx>
#include <gp_Pnt.hxx>
#include "hs_Exception.h"
#include "hs_IntCurvesFace_ShapeIntersector.h"

IntCurvesFace_ShapeIntersector * hs_new_IntCurvesFace_ShapeIntersector(){
    return new IntCurvesFace_ShapeIntersector();
}

void hs_delete_IntCurvesFace_ShapeIntersector(IntCurvesFace_ShapeIntersector * intersector){
    delete intersector;
}

void hs_IntCurvesFace_ShapeIntersector_Load(IntCurvesFace_ShapeIntersector * intersector, TopoDS_Shape * shape, double tol, HSExceptionType* exType, void ** exPtr){
    hs_handleExVoid(exType, exPtr, [intersector, shape, tol]{
        intersector->Load(*shape, tol);
    });
}

void hs_IntCurvesFace_ShapeIntersector_Perform(IntCurvesFace_ShapeIntersector * intersector, gp_Lin * line, double pInf, double pSup, HSExceptionType* exType, void ** exPtr){
    hs_handleExVoid(exType, exPtr, [intersector, line, pInf, pSup]{
        intersector->Perform(*line, pInf, pSup);
    });
}

void hs_IntCurvesFace_ShapeIntersector_PerformNearest(IntCurvesFace_ShapeIntersector * intersector, gp_Lin * line, double pInf, double pSup, HSExceptionType* exType, void ** exPtr){
    hs_handleExVoid(exType, exPtr, [intersector, line, pInf, pSup]{
        intersector->PerformNearest(*line, pInf, pSup);
    });
}

bool hs_IntCurvesFace_ShapeIntersector_IsDone(IntCurvesFace_ShapeIntersector * intersector){
    return intersector->IsDone();
}

int hs_IntCurvesFace_ShapeIntersector_NbPnt(IntCurvesFace_ShapeIntersector * intersector){
    return intersector->NbPnt();
}

double hs_IntCurvesFace_ShapeIntersector_UParameter(IntCurvesFace_ShapeIntersector * intersector, int index, HSExceptionType* exType, void ** exPtr){
    return hs_handleExWithDefault(exType, exPtr, [intersector, index]{
        return intersector->UParameter(index);
    }, 0.0);
}

double hs_IntCurvesFace_ShapeIntersector_VParameter(IntCurvesFace_ShapeIntersector * intersector, int index, HSExceptionType* exType, void ** exPtr){
    return hs_handleExWithDefault(exType, exPtr, [intersector, index]{
        return intersector->VParameter(index);
    }, 0.0);
}

double hs_IntCurvesFace_ShapeIntersector_WParameter(IntCurvesFace_ShapeIntersector * intersector, int index, HSExceptionType* exType, void ** exPtr){
    return hs_handleExWithDefault(exType, exPtr, [intersector, index]{
        return intersector->WParameter(index);
    }, 0.0);
}

gp_Pnt * hs_IntCurvesFace_ShapeIntersector_Pnt(IntCurvesFace_ShapeIntersector * intersector, int index, HSExceptionType* exType, void ** exPtr){
    return hs_handleEx(exType, exPtr, [intersector, index]{
        return new gp_Pnt(intersector->Pnt(index));
    });
}

TopoDS_Face * hs_IntCurvesFace_ShapeIntersector_Face(IntCurvesFace_ShapeIntersector * intersector, int index, HSExceptionType* exType, void ** exPtr){
    return hs_handleEx(exType, exPtr, [intersector, index]{
        return new TopoDS_Face(intersector->Face(index));
    });
}
