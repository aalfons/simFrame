#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Call calls */
extern SEXP R_inclusionProb(SEXP R_prob, SEXP R_size);
extern SEXP R_tille(SEXP R_prob);
extern SEXP R_brewer(SEXP R_prob);

static const R_CallMethodDef CallEntries[] = {
  {"R_inclusionProb", (DL_FUNC) &R_inclusionProb, 2},
  {"R_tille",         (DL_FUNC) &R_tille,         1},
  {"R_brewer",        (DL_FUNC) &R_brewer,        1},
  {NULL, NULL, 0}
};

void R_init_simFrame(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
