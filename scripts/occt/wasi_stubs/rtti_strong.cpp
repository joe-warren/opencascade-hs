// Force strong definitions of OCCT exception typeinfo symbols.
// Without this, each .o file that includes OCCT exception headers gets
// its own weak typeinfo definition, and wasm-ld's R_WASM_MEMORY_ADDR_REL_SLEB
// relocation for weak symbols is buggy in PIC shared libraries.
//
// By providing explicit definitions here (compiled with the same flags as
// everything else), all typeinfo references resolve to one consistent location.

#include <Standard_DomainError.hxx>
#include <Standard_RangeError.hxx>
#include <Standard_OutOfRange.hxx>
#include <Standard_NoSuchObject.hxx>
#include <Standard_TypeMismatch.hxx>
#include <Standard_ConstructionError.hxx>
#include <Standard_DimensionError.hxx>
#include <Standard_NullObject.hxx>
#include <Standard_NumericError.hxx>
#include <Standard_Overflow.hxx>
#include <Standard_Underflow.hxx>
#include <Standard_DivideByZero.hxx>
#include <Standard_ProgramError.hxx>
#include <Standard_NotImplemented.hxx>
#include <Standard_OutOfMemory.hxx>

// Force vtable and typeinfo emission by defining a dummy function
// that uses each type. The compiler must emit strong typeinfo for
// types whose vtable is referenced.
namespace {
    template<typename T>
    __attribute__((used)) void force_typeinfo() {
        T e("force");
        (void)e.what();
    }

    __attribute__((constructor))
    void init_rtti() {
        // These calls ensure the compiler emits typeinfo for each type.
        // They never actually run (the constructor is a no-op in practice)
        // but the compiler can't prove that, so it keeps the typeinfo.
        if (false) {
            force_typeinfo<Standard_DomainError>();
            force_typeinfo<Standard_RangeError>();
            force_typeinfo<Standard_OutOfRange>();
            force_typeinfo<Standard_NoSuchObject>();
            force_typeinfo<Standard_TypeMismatch>();
            force_typeinfo<Standard_ConstructionError>();
            force_typeinfo<Standard_DimensionError>();
            force_typeinfo<Standard_NullObject>();
            force_typeinfo<Standard_NumericError>();
            force_typeinfo<Standard_Overflow>();
            force_typeinfo<Standard_Underflow>();
            force_typeinfo<Standard_DivideByZero>();
            force_typeinfo<Standard_ProgramError>();
            force_typeinfo<Standard_NotImplemented>();
            force_typeinfo<Standard_OutOfMemory>();
        }
    }
}
