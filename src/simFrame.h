/*
 * Author: Andreas Alfons
 *         Erasmus University Rotterdam
 */

#ifndef _simFrame_H
#define _simFrame_H

#include <Rcpp.h>

RcppExport SEXP R_inclusionProb(SEXP R_prob, SEXP R_size);
RcppExport SEXP R_tille(SEXP R_prob);
RcppExport SEXP R_brewer(SEXP R_prob);

#endif
