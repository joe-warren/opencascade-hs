#include <Font_BRepFont.hxx>
#include "hs_Exception.h"
#include "hs_Font_BRepFont.h"


Font_BRepFont * hs_new_Font_BRepFont(){
    return new Font_BRepFont();
}

Font_BRepFont * hs_new_Font_BRepFont_fromStringAndSize(
        char * path, double size,
        HSExceptionType* exType, void ** exPtr
){
    return hs_handleEx(
        exType,
        exPtr,
        [path, size]{
            return new Font_BRepFont(path, size);
        }
    );
}

void hs_delete_Font_BRepFont(Font_BRepFont * font){
    delete font;
}

bool hs_Font_BRepFont_initPathAndSize(
        Font_BRepFont * font, char * path, double size,
        HSExceptionType* exType, void ** exPtr
){
    return hs_handleExWithDefault(
        exType,
        exPtr,
        [font, path, size]{
            return font->Init(path, size, 0);
        },
        false
    );
}

bool hs_Font_BRepFont_initNameAspectAndSize(
        Font_BRepFont * font, char * name, Font_FontAspect aspect, double size,
        HSExceptionType* exType, void ** exPtr
){
    return hs_handleExWithDefault(
        exType,
        exPtr,
        [font, name, aspect, size]{
            return font->Init(name, aspect, size);
        },
        false
    );
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