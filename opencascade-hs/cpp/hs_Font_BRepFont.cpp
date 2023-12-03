#include <Font_BRepFont.hxx>
#include "hs_Font_BRepFont.h"


Font_BRepFont * hs_new_Font_BRepFont(){
    return new Font_BRepFont();
}

Font_BRepFont * hs_new_Font_BRepFont_fromStringAndSize(char * path, double size){
    return new Font_BRepFont(path, size);
}

void hs_delete_Font_BRepFont(Font_BRepFont * font){
    delete font;
}

bool hs_Font_BRepFont_initPathAndSize(Font_BRepFont * font, char * path, double size){
    return font->Init(path, size, 0);
}

bool hs_Font_BRepFont_initNameAspectAndSize(Font_BRepFont * font, char * name, Font_FontAspect aspect, double size){
    return font->Init(name, aspect, size);
}

double hs_Font_BRepFont_ascender(Font_BRepFont * font){
    return font->Ascender();
}

double hs_Font_BRepFont_descender(Font_BRepFont * font){
    return font->Descender();
}

double hs_Font_BRepFont_lineSpacing(Font_BRepFont * font){
    return font->LineSpacing();
}

double hs_Font_BRepFont_pointSize(Font_BRepFont * font){
    return font->LineSpacing();
}