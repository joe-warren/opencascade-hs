#ifndef HS_TOPODS_SHAPE_H
#define HS_TOPODS_SHAPE_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

TopoDS_Shape * hs_new_TopoDS_Shape();

void hs_delete_TopoDS_Shape(TopoDS_Shape * shape);

bool hs_TopoDS_Shape_IsNull(TopoDS_Shape * shape);

void hs_TopoDS_Shape_Nullify(TopoDS_Shape * shape);

TopLoc_Location * hs_TopoDS_Shape_Location(TopoDS_Shape * shape);

void hs_TopoDS_Shape_SetLocation(TopoDS_Shape * shape, TopLoc_Location * location);

TopoDS_Shape * hs_TopoDS_Shape_Located(TopoDS_Shape * shape, TopLoc_Location * location);

TopAbs_Orientation hs_TopoDS_Shape_Orientation(TopoDS_Shape * shape);

void hs_TopoDS_Shape_SetOrientation(TopoDS_Shape * shape, TopAbs_Orientation orientation);

TopoDS_Shape * hs_TopoDS_Shape_Oriented(TopoDS_Shape * shape, TopAbs_Orientation orientation);

TopAbs_ShapeEnum hs_TopoDS_Shape_ShapeType(TopoDS_Shape * shape);

bool hs_TopoDS_Shape_Free(TopoDS_Shape * shape);

void hs_TopoDS_Shape_SetFree(TopoDS_Shape * shape, bool b);

bool hs_TopoDS_Shape_Locked(TopoDS_Shape * shape);

void hs_TopoDS_Shape_SetLocked(TopoDS_Shape * shape, bool b);

bool hs_TopoDS_Shape_Modified(TopoDS_Shape * shape);

void hs_TopoDS_Shape_SetModified(TopoDS_Shape * shape, bool b);


bool hs_TopoDS_Shape_Checked(TopoDS_Shape * shape);

void hs_TopoDS_Shape_SetChecked(TopoDS_Shape * shape, bool b);


bool hs_TopoDS_Shape_Orientable(TopoDS_Shape * shape);

void hs_TopoDS_Shape_SetOrientable(TopoDS_Shape * shape, bool b);

bool hs_TopoDS_Shape_Closed(TopoDS_Shape * shape);

void hs_TopoDS_Shape_SetClosed(TopoDS_Shape * shape, bool b);


bool hs_TopoDS_Shape_Infinite(TopoDS_Shape * shape);

void hs_TopoDS_Shape_SetInfinite(TopoDS_Shape * shape, bool b);


bool hs_TopoDS_Shape_Convex(TopoDS_Shape * shape);

void hs_TopoDS_Shape_SetConvex(TopoDS_Shape * shape, bool b);

void hs_TopoDS_Shape_Move(TopoDS_Shape * shape, TopLoc_Location * position);

TopoDS_Shape * hs_TopoDS_Shape_Moved(TopoDS_Shape * shape, TopLoc_Location * position);

int hs_TopoDS_Shape_NbChildren(TopoDS_Shape * shape);

void hs_TopoDS_Shape_Reverse(TopoDS_Shape * shape);

TopoDS_Shape * hs_TopoDS_Shape_Reversed(TopoDS_Shape * shape);

void hs_TopoDS_Shape_Complement(TopoDS_Shape * shape);

TopoDS_Shape * hs_TopoDS_Shape_Complemented(TopoDS_Shape * shape);

void hs_TopoDS_Shape_Compose(TopoDS_Shape * shape, TopAbs_Orientation orient);

TopoDS_Shape * hs_TopoDS_Shape_Composed(TopoDS_Shape * shape, TopAbs_Orientation orient);

bool hs_TopoDS_Shape_IsEqual(TopoDS_Shape * a, TopoDS_Shape* b);

bool hs_TopoDS_Shape_IsSame(TopoDS_Shape * a, TopoDS_Shape* b);

bool hs_TopoDS_Shape_IsPartner(TopoDS_Shape * a, TopoDS_Shape* b);

bool hs_TopoDS_Shape_IsNotEqual(TopoDS_Shape * a, TopoDS_Shape* b);

void hs_TopoDS_Shape_EmptyCopy(TopoDS_Shape * shape);

TopoDS_Shape * hs_TopoDS_Shape_EmptyCopied(TopoDS_Shape * shape);

#ifdef __cplusplus
}
#endif

#endif // HS_TOPODS_SHAPE_H
