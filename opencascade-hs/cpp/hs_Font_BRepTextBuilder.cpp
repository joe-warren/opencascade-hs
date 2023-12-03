#include <Font_BRepTextBuilder.hxx>
#include "hs_Font_BRepTextBuilder.h"

Font_BRepTextBuilder * hs_new_Font_BRepTextBuilder(){
    return new Font_BRepTextBuilder();
}

void hs_delete_Font_BRepTextBuilder(Font_BRepTextBuilder * builder){
    delete builder;
}

TopoDS_Shape * hs_Font_BRepTextBuilder_perform(
        Font_BRepTextBuilder * builder, 
        Font_BRepFont * font,
        char * theString, 
        gp_Ax3 * thePenLoc,
        Graphic3d_HorizontalTextAlignment theHAlign,
        Graphic3d_VerticalTextAlignment theVAlign
    ) {
    return new TopoDS_Shape (builder ->Perform(*font, theString, *thePenLoc, theHAlign, theVAlign));
}