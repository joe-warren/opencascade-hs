// Default the playground to OCCT's Delabella mesh algorithm.
//
// The default Watson algorithm in OCCT 8.0-dev produces corrupt
// triangulations for boolean-result faces with internal boundaries: interior
// nodes are generated but never inserted, and the node-index compaction then
// maps triangle indices onto wrong/uninitialised nodes (same family as OCCT
// issue #929 / PR #940, which did not fully fix this case - reproducible
// natively, nothing wasm-specific). Delabella triangulates these faces
// correctly.
//
// setenv with overwrite=0 keeps CSF_MeshAlgo overridable from outside.
#include <cstdlib>

__attribute__((constructor)) static void waterfall_default_mesh_algo(void)
{
    setenv("CSF_MeshAlgo", "delabella", 0);
}
