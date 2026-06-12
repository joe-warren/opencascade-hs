#include <TopoDS_Shape.hxx>
#include "hs_Exception.h"
#include "hs_TopoDS_Shape.h"

TopoDS_Shape * hs_new_TopoDS_Shape(){
    return new TopoDS_Shape();
}

TopoDS_Shape * hs_new_TopoDS_Shape_copy(TopoDS_Shape * shape){
    return new TopoDS_Shape(*shape);
}

void hs_delete_TopoDS_Shape(TopoDS_Shape * shape){
    delete shape;
}

bool hs_TopoDS_Shape_IsNull(TopoDS_Shape * shape, HSExceptionType* exType, void ** exPtr){
    return hs_handleExWithDefault(exType, exPtr, [shape]{
        return shape->IsNull();
    }, false);
}

void hs_TopoDS_Shape_Nullify(TopoDS_Shape * shape, HSExceptionType* exType, void ** exPtr){
    hs_handleExVoid(exType, exPtr, [shape]{
        shape->Nullify();
    });
}

TopLoc_Location * hs_TopoDS_Shape_Location(TopoDS_Shape * shape, HSExceptionType* exType, void ** exPtr){
    return hs_handleEx(exType, exPtr, [shape]{
        return new TopLoc_Location(shape->Location());
    });
}

void hs_TopoDS_Shape_SetLocation(TopoDS_Shape * shape, TopLoc_Location * location, HSExceptionType* exType, void ** exPtr){
    hs_handleExVoid(exType, exPtr, [shape, location]{
        shape->Location(*location);
    });
}

TopoDS_Shape * hs_TopoDS_Shape_Located(TopoDS_Shape * shape, TopLoc_Location * location, HSExceptionType* exType, void ** exPtr){
    return hs_handleEx(exType, exPtr, [shape, location]{
        return new TopoDS_Shape(shape->Located(*location));
    });
}

TopAbs_Orientation hs_TopoDS_Shape_Orientation(TopoDS_Shape * shape, HSExceptionType* exType, void ** exPtr){
    return hs_handleExWithDefault(exType, exPtr, [shape]{
        return shape->Orientation();
    }, static_cast<TopAbs_Orientation>(0));
}

void hs_TopoDS_Shape_SetOrientation(TopoDS_Shape * shape, TopAbs_Orientation orientation, HSExceptionType* exType, void ** exPtr){
    hs_handleExVoid(exType, exPtr, [shape, orientation]{
        shape->Orientation(orientation);
    });
}

TopoDS_Shape * hs_TopoDS_Shape_Oriented(TopoDS_Shape * shape, TopAbs_Orientation orientation, HSExceptionType* exType, void ** exPtr){
    return hs_handleEx(exType, exPtr, [shape, orientation]{
        return new TopoDS_Shape(shape->Oriented(orientation));
    });
}

TopAbs_ShapeEnum hs_TopoDS_Shape_ShapeType(TopoDS_Shape * shape, HSExceptionType* exType, void ** exPtr){
    return hs_handleExWithDefault(exType, exPtr, [shape]{
        return shape->ShapeType();
    }, static_cast<TopAbs_ShapeEnum>(0));
}

bool hs_TopoDS_Shape_Free(TopoDS_Shape * shape, HSExceptionType* exType, void ** exPtr){
    return hs_handleExWithDefault(exType, exPtr, [shape]{
        return shape->Free();
    }, false);
}

void hs_TopoDS_Shape_SetFree(TopoDS_Shape * shape, bool b, HSExceptionType* exType, void ** exPtr){
    hs_handleExVoid(exType, exPtr, [shape, b]{
        shape->Free(b);
    });
}


bool hs_TopoDS_Shape_Locked(TopoDS_Shape * shape, HSExceptionType* exType, void ** exPtr){
    return hs_handleExWithDefault(exType, exPtr, [shape]{
        return shape->Locked();
    }, false);
}

void hs_TopoDS_Shape_SetLocked(TopoDS_Shape * shape, bool b, HSExceptionType* exType, void ** exPtr){
    hs_handleExVoid(exType, exPtr, [shape, b]{
        shape->Locked(b);
    });
}

bool hs_TopoDS_Shape_Modified(TopoDS_Shape * shape, HSExceptionType* exType, void ** exPtr){
    return hs_handleExWithDefault(exType, exPtr, [shape]{
        return shape->Modified();
    }, false);
}

void hs_TopoDS_Shape_SetModified(TopoDS_Shape * shape, bool b, HSExceptionType* exType, void ** exPtr){
    hs_handleExVoid(exType, exPtr, [shape, b]{
        shape->Modified(b);
    });
}


bool hs_TopoDS_Shape_Checked(TopoDS_Shape * shape, HSExceptionType* exType, void ** exPtr){
    return hs_handleExWithDefault(exType, exPtr, [shape]{
        return shape->Checked();
    }, false);
}

void hs_TopoDS_Shape_SetChecked(TopoDS_Shape * shape, bool b, HSExceptionType* exType, void ** exPtr){
    hs_handleExVoid(exType, exPtr, [shape, b]{
        shape->Checked(b);
    });
}


bool hs_TopoDS_Shape_Orientable(TopoDS_Shape * shape, HSExceptionType* exType, void ** exPtr){
    return hs_handleExWithDefault(exType, exPtr, [shape]{
        return shape->Orientable();
    }, false);
}

void hs_TopoDS_Shape_SetOrientable(TopoDS_Shape * shape, bool b, HSExceptionType* exType, void ** exPtr){
    hs_handleExVoid(exType, exPtr, [shape, b]{
        shape->Orientable(b);
    });
}

bool hs_TopoDS_Shape_Closed(TopoDS_Shape * shape, HSExceptionType* exType, void ** exPtr){
    return hs_handleExWithDefault(exType, exPtr, [shape]{
        return shape->Closed();
    }, false);
}

void hs_TopoDS_Shape_SetClosed(TopoDS_Shape * shape, bool b, HSExceptionType* exType, void ** exPtr){
    hs_handleExVoid(exType, exPtr, [shape, b]{
        shape->Closed(b);
    });
}


bool hs_TopoDS_Shape_Infinite(TopoDS_Shape * shape, HSExceptionType* exType, void ** exPtr){
    return hs_handleExWithDefault(exType, exPtr, [shape]{
        return shape->Infinite();
    }, false);
}

void hs_TopoDS_Shape_SetInfinite(TopoDS_Shape * shape, bool b, HSExceptionType* exType, void ** exPtr){
    hs_handleExVoid(exType, exPtr, [shape, b]{
        shape->Infinite(b);
    });
}


bool hs_TopoDS_Shape_Convex(TopoDS_Shape * shape, HSExceptionType* exType, void ** exPtr){
    return hs_handleExWithDefault(exType, exPtr, [shape]{
        return shape->Convex();
    }, false);
}

void hs_TopoDS_Shape_SetConvex(TopoDS_Shape * shape, bool b, HSExceptionType* exType, void ** exPtr){
    hs_handleExVoid(exType, exPtr, [shape, b]{
        shape->Convex(b);
    });
}

void hs_TopoDS_Shape_Move(TopoDS_Shape * shape, TopLoc_Location * position, HSExceptionType* exType, void ** exPtr){
    hs_handleExVoid(exType, exPtr, [shape, position]{
        shape->Move(*position);
    });
}

TopoDS_Shape * hs_TopoDS_Shape_Moved(TopoDS_Shape * shape, TopLoc_Location * position, HSExceptionType* exType, void ** exPtr){
    return hs_handleEx(exType, exPtr, [shape, position]{
        return new TopoDS_Shape(shape->Moved(*position));
    });
}

int hs_TopoDS_Shape_NbChildren(TopoDS_Shape * shape, HSExceptionType* exType, void ** exPtr){
    return hs_handleExWithDefault(exType, exPtr, [shape]{
        return shape->NbChildren();
    }, 0);
}

void hs_TopoDS_Shape_Reverse(TopoDS_Shape * shape, HSExceptionType* exType, void ** exPtr){
    hs_handleExVoid(exType, exPtr, [shape]{
        shape->Reverse();
    });
}

TopoDS_Shape * hs_TopoDS_Shape_Reversed(TopoDS_Shape * shape, HSExceptionType* exType, void ** exPtr){
    return hs_handleEx(exType, exPtr, [shape]{
        return new TopoDS_Shape(shape->Reversed());
    });
}


void hs_TopoDS_Shape_Complement(TopoDS_Shape * shape, HSExceptionType* exType, void ** exPtr){
    hs_handleExVoid(exType, exPtr, [shape]{
        shape->Complement();
    });
}

TopoDS_Shape * hs_TopoDS_Shape_Complemented(TopoDS_Shape * shape, HSExceptionType* exType, void ** exPtr){
    return hs_handleEx(exType, exPtr, [shape]{
        return new TopoDS_Shape(shape->Complemented());
    });
}

void hs_TopoDS_Shape_Compose(TopoDS_Shape * shape, TopAbs_Orientation orient, HSExceptionType* exType, void ** exPtr){
    hs_handleExVoid(exType, exPtr, [shape, orient]{
        shape->Compose(orient);
    });
}

TopoDS_Shape * hs_TopoDS_Shape_Composed(TopoDS_Shape * shape, TopAbs_Orientation orient, HSExceptionType* exType, void ** exPtr){
    return hs_handleEx(exType, exPtr, [shape, orient]{
        return new TopoDS_Shape(shape->Composed(orient));
    });
}

bool hs_TopoDS_Shape_IsEqual(TopoDS_Shape * a, TopoDS_Shape* b, HSExceptionType* exType, void ** exPtr){
    return hs_handleExWithDefault(exType, exPtr, [a, b]{
        return a->IsEqual(*b);
    }, false);
}

bool hs_TopoDS_Shape_IsSame(TopoDS_Shape * a, TopoDS_Shape* b, HSExceptionType* exType, void ** exPtr){
    return hs_handleExWithDefault(exType, exPtr, [a, b]{
        return a->IsSame(*b);
    }, false);
}

bool hs_TopoDS_Shape_IsPartner(TopoDS_Shape * a, TopoDS_Shape* b, HSExceptionType* exType, void ** exPtr){
    return hs_handleExWithDefault(exType, exPtr, [a, b]{
        return a->IsPartner(*b);
    }, false);
}

bool hs_TopoDS_Shape_IsNotEqual(TopoDS_Shape * a, TopoDS_Shape* b, HSExceptionType* exType, void ** exPtr){
    return hs_handleExWithDefault(exType, exPtr, [a, b]{
        return a->IsNotEqual(*b);
    }, false);
}


void hs_TopoDS_Shape_EmptyCopy(TopoDS_Shape * shape, HSExceptionType* exType, void ** exPtr){
    hs_handleExVoid(exType, exPtr, [shape]{
        shape->EmptyCopy();
    });
}

TopoDS_Shape * hs_TopoDS_Shape_EmptyCopied(TopoDS_Shape * shape, HSExceptionType* exType, void ** exPtr){
    return hs_handleEx(exType, exPtr, [shape]{
        return new TopoDS_Shape(shape->EmptyCopied());
    });
}
