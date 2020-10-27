#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _DataVisualizations_c_inPSphere2D(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _DataVisualizations_c_pde(SEXP, SEXP, SEXP, SEXP);
extern SEXP _DataVisualizations_c_quantile(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_DataVisualizations_c_inPSphere2D", (DL_FUNC) &_DataVisualizations_c_inPSphere2D, 7},
    {"_DataVisualizations_c_pde",         (DL_FUNC) &_DataVisualizations_c_pde,         4},
    {"_DataVisualizations_c_quantile",    (DL_FUNC) &_DataVisualizations_c_quantile,    2},
    {NULL, NULL, 0}
};

void R_init_DataVisualizations(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
/* 
output of call
tools::package_native_routine_registration_skeleton(".", character_only = FALSE)
*/