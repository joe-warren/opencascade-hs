#ifndef HS_INTCURVESFACE_SHAPEINTERSECTOR_H
#define HS_INTCURVESFACE_SHAPEINTERSECTOR_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

IntCurvesFace_ShapeIntersector * hs_new_IntCurvesFace_ShapeIntersector();

void hs_delete_IntCurvesFace_ShapeIntersector(IntCurvesFace_ShapeIntersector * intersector);

void hs_IntCurvesFace_ShapeIntersector_Load(IntCurvesFace_ShapeIntersector * intersector, TopoDS_Shape * shape, double tol, HSExceptionType* exType, void ** exPtr);

void hs_IntCurvesFace_ShapeIntersector_Perform(IntCurvesFace_ShapeIntersector * intersector, gp_Lin * line, double pInf, double pSup, HSExceptionType* exType, void ** exPtr);

void hs_IntCurvesFace_ShapeIntersector_PerformNearest(IntCurvesFace_ShapeIntersector * intersector, gp_Lin * line, double pInf, double pSup, HSExceptionType* exType, void ** exPtr);

bool hs_IntCurvesFace_ShapeIntersector_IsDone(IntCurvesFace_ShapeIntersector * intersector);

int hs_IntCurvesFace_ShapeIntersector_NbPnt(IntCurvesFace_ShapeIntersector * intersector);

double hs_IntCurvesFace_ShapeIntersector_UParameter(IntCurvesFace_ShapeIntersector * intersector, int index, HSExceptionType* exType, void ** exPtr);

double hs_IntCurvesFace_ShapeIntersector_VParameter(IntCurvesFace_ShapeIntersector * intersector, int index, HSExceptionType* exType, void ** exPtr);

double hs_IntCurvesFace_ShapeIntersector_WParameter(IntCurvesFace_ShapeIntersector * intersector, int index, HSExceptionType* exType, void ** exPtr);

gp_Pnt * hs_IntCurvesFace_ShapeIntersector_Pnt(IntCurvesFace_ShapeIntersector * intersector, int index, HSExceptionType* exType, void ** exPtr);

TopoDS_Face * hs_IntCurvesFace_ShapeIntersector_Face(IntCurvesFace_ShapeIntersector * intersector, int index, HSExceptionType* exType, void ** exPtr);

#ifdef __cplusplus
}
#endif

#endif // HS_INTCURVESFACE_SHAPEINTERSECTOR_H
