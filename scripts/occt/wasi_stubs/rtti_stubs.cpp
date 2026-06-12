/* Stubs for C++ RTTI typeinfo symbols that leak through --whole-archive.
 * These are referenced by exception handling code but never actually used
 * at runtime with No_Exception defined.
 *
 * The symbols are GOT.mem imports (global offset table entries for the
 * typeinfo objects). We just need them to exist as non-null pointers.
 *
 * Add new entries here as needed - any _ZTI symbol that shows up as
 * an unresolved GOT.mem import when linking with --whole-archive.
 */

static char dummy[64] = {};

#define RTTI_STUB(mangled) \
  __attribute__((visibility("default"))) void* mangled = &dummy;

RTTI_STUB(_ZTI20Standard_DomainError)
RTTI_STUB(_ZTI18NCollection_Array1I6gp_PntE)
RTTI_STUB(_ZTI19NCollection_BaseMap)
RTTI_STUB(_ZTI20NCollection_BaseList)
