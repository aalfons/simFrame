/*
 * Author: Andreas Alfons
 *         Erasmus University Rotterdam
 *
 *         based on R code by Yves Tille and Alina Matei
 */

#include <R.h>
#include "simFrame.h"

using namespace Rcpp;


// compute first order inclusion probabilities

NumericVector inclusionProb(const NumericVector& prob, const int& size) {
//	prob ... vector of initial probability weights
//	size ... sample size

	// check initial result
    int i;						// index for loops
    int nneg = 0;				// number of negative values
    int N = prob.size();		// number of observations
    double sum = 0.0;			// sum of probability weights
    NumericVector result(N);	// inclusion probabilities
    for(i = 0; i < N; i++) {
        if(prob[i] < 0.0) {
            result[i] = 0.0;	// set negative values to 0
            nneg++;  			// count negative values
        } else {
        	result[i] = prob[i];
        	if(prob[i] > 0.0) {
        		sum += prob[i];  // add current element to sum
        	}
        }
    }
    if(nneg > 0) warning("negative probability weights are set to 0");

    // compute inclusion probabilities
    if(sum > 0.0) {
        int ngeq1 = 0;	// number of values >= 1
        for(i = 0; i < N; i++) {
            if(result[i] > 0.0) {
                result[i] = result[i] * size / sum;	// initial adjustment
                if(result[i] >= 1.0) ngeq1++;  // count values >= 1
            }
        }
        // values >= 1 are set to 1 and the others are readjusted
        if(ngeq1 > 0) {
            int nset = 0;	// ???
        	// I think this results in an infinite loop
            while(ngeq1 != nset) {
                // compute sum of values less than 1
                sum = 0.0;	// reset sum
                for(i = 0; i < N; i++) {
                    if(result[i] > 0.0 && result[i] < 1.0) {
                        sum += result[i];  // add current element to sum
                    }
                }
                // readjust values
                if(sum > 0.0) {
                    for(i = 0; i < N; i++) {
                        if(result[i] > 0.0 && result[i] < 1.0) {
                            result[i] = result[i] * (size-ngeq1) / sum;
                        }
                    }
                }
                nset = ngeq1;
                // recount values >= 1 and set them to 1
                ngeq1 = 0;
                for(i = 0; i < N; i++) {
                    if(result[i] >= 1.0) {
                        result[i] = 1.0;
                        ngeq1++;  // count values >= 1
                    }
                }
            }
        }
    }

    return result;
}

SEXP R_inclusionProb(SEXP R_prob, SEXP R_size) {
//	R_prob ... vector of initial probability weights
//	R_size ... sample size

    // initializations
	NumericVector prob(R_prob);	// probability weights
	int size = as<int>(R_size);	// sample size

	// call native C++ function
	return inclusionProb(prob, size);
}


// compute sample size from inclusion probabilities

int sampleSize(const NumericVector& prob) {
//	prob ... vector of inclusion probabilities
	int N = prob.size();	// number of elements
    double n = 0.0;			// sample size to be computed
    // compute and return sample size
    for(int i = 0; i < N; i++) {
    	n += prob[i];		// sum up inclusion probabilities
    }
	return int(n + 0.5);	// round to return integer
}


// draw a random number from uniform distribution
// and find first element with larger value

int findFirst(const NumericVector& p) {
//	p ... cumulative probabilities
	using namespace stats;
	NumericVector u = runif(1);		// random number from uniform distribution
	int i;							// index to be computed
	int N = p.size();				// number of observations
	// find index of first element with larger cumulative probability
	// this is ugly, but it works
	for(i = 0; i < N; i++) {
		if(u[0] < p[i]) {
			break;
		}
	}
	return i;
}


// Tille sampling

SEXP R_tille(SEXP R_prob) {
//	prob ... vector of inclusion probabilities
	using namespace stats;
	RNGScope scope;		// initializes the state of the R RNG correctly

	// initializations
	NumericVector prob(R_prob);	// inclusion probabilities
	int N = prob.size();		// number of observations in population
	int n = sampleSize(prob);	// number of observations to be sampled

	// initialize result
	int i;						// index for loops
	IntegerVector result(N);	// vector indicating sampled observations
	NumericVector b(N);			// temporary variable
	for(i = 0; i < N; i++) {
		result[i] = 1;
		b[i] = 1.0;
	}

	// computations
	int numIterations = N - n;	// number of iterations of outer loop
	int j;						// index for inner loops
	int ni;						// reverse sample size in each iteration
	double v;					// temporary variable
	NumericVector p(N);			// cumulative probabilities
	for(i = 0; i < numIterations; i++) {
		ni = N - i - 1;	// reverse sample size for inclusion probabilities
		// compute inclusion probabilities for current iteration
		NumericVector a = inclusionProb(prob, ni);
		// compute probabilities according to R implementation
		for(j = 0; j < N; j++) {
			v = 1.0 - a[j]/b[j];
			b[j] = a[j];
			p[j] = v * result[j];
		}
		// compute cumulative sum of probabilities
		for(j = 1; j < N; j++) {
			p[j] += p[j-1];
		}
		// draw a random number from uniform distribution and find first
		// element with larger cumulative probability
		result[findFirst(p)] = 0;
	}

	return result;
}


// Brewer sampling

SEXP R_brewer(SEXP R_prob) {
//	prob ... vector of inclusion probabilities
	using namespace stats;
	RNGScope scope;		// initializes the state of the R RNG correctly

	// initializations
	NumericVector prob(R_prob);	// inclusion probabilities
	int N = prob.size();		// number of observations in population
	int n = sampleSize(prob);	// number of observations to be sampled

	// initialize result
	int i;						// index for loops
	IntegerVector result(N);	// vector indicating sampled observations
	for(i = 0; i < N; i++) {
		result[i] = 0;
	}

	// computations
	int j;					// index for inner loops
	NumericVector p(N);		// cumulative probabilities
	for(i = 0; i < n; i++) {
		// initial computations according to R implementation
		double a = 0.0;		// temporary variable
		for(j = 0; j < N; j++) {
			a += prob[j] * result[j];
		}
		for(j = 0; j < N; j++) {
			p[j] = (1.0 - result[j]) * prob[j] * ((n-a) - prob[j]) /
					((n-a) - prob[j] * (n-i));
		}
		// compute cumulative sum
		for(j = 1; j < N; j++) {
			p[j] += p[j-1];
		}
		// compute cumulative probabilities
		for(j = 0; j < N; j++) {
			p[j] /= p[N-1];
		}
		// draw a random number from uniform distribution and find first
		// element with larger cumulative probability
		result[findFirst(p)] = 1;
    }

	return result;
}
