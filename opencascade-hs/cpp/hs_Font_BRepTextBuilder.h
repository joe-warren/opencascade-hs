#ifndef HS_FONT_BREPTEXTBUILDER_H
#define HS_FONT_BREPTEXTBUILDER_H

#include "hs_types.h"

#ifdef __cplusplus
extern "C" {
#endif

Font_BRepTextBuilder * hs_new_Font_BRepTextBuilder();

void hs_delete_Font_BRepTextBuilder(Font_BRepTextBuilder * builder);

TopoDS_Shape * hs_Font_BRepTextBuilder_perform(
        Font_BRepTextBuilder * builder, 
        Font_BRepFont * font,
        char * theString, 
        gp_Ax3 * thePenLoc,
        Graphic3d_HorizontalTextAlignment theHAlign,
        Graphic3d_VerticalTextAlignment theVAlign
    );
#ifdef __cplusplus
}
#endif

#endif // HS_FONT_BREPFONT_H
