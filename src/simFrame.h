/*
 * Author: Andreas Alfons
 *         KU Leuven
 */

#ifndef _simFrame_H
#define _simFrame_H

#include <Rcpp.h>

//RcppExport SEXP inclusionProb(SEXP prob, SEXP size);
//RcppExport SEXP tille(SEXP prob);
//RcppExport SEXP brewer(SEXP prob);
RcppExport SEXP R_inclusionProb(SEXP R_prob, SEXP R_size);
RcppExport SEXP R_tille(SEXP R_prob);
RcppExport SEXP R_brewer(SEXP R_prob);

#endif
