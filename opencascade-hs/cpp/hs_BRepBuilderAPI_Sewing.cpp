#include <BRepBuilderAPI_Sewing.hxx>
#include "hs_BRepBuilderAPI_Sewing.h"

BRepBuilderAPI_Sewing * hs_new_BRepBuilderAPI_Sewing(double tolerance , bool option1, bool option2, bool option3, bool option4){
    return new BRepBuilderAPI_Sewing(tolerance, option1, option2, option3, option4);
}

void hs_delete_BRepBuilderAPI_Sewing(BRepBuilderAPI_Sewing * builder){
    delete builder;
}

void hs_BRepBuilderAPI_Sewing_load(BRepBuilderAPI_Sewing * builder, TopoDS_Shape * shape){
    builder->Load(*shape);
}

void hs_BRepBuilderAPI_Sewing_add(BRepBuilderAPI_Sewing * builder, TopoDS_Shape * shape){
    builder->Add(*shape);
}

void hs_BRepBuilderAPI_Sewing_perform(BRepBuilderAPI_Sewing * builder){
    builder->Perform();
}

TopoDS_Shape * hs_BRepBuilderAPI_Sewing_sewedShape(BRepBuilderAPI_Sewing * builder){
    return new TopoDS_Shape(builder->SewedShape());
}


int hs_BRepBuilderAPI_Sewing_nbFreeEdges(BRepBuilderAPI_Sewing * builder){
    return builder->NbFreeEdges();
}


int hs_BRepBuilderAPI_Sewing_nbContigousEdges(BRepBuilderAPI_Sewing * builder){
    return builder->NbContigousEdges();
}

int hs_BRepBuilderAPI_Sewing_nbMultipleEdges(BRepBuilderAPI_Sewing * builder){
    return builder->NbMultipleEdges();
}