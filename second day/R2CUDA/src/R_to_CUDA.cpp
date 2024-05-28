#include </apps/all/R/4.1.0-foss-2021a/lib/R/library/Rcpp/include/Rcpp.h>
#include <stdio.h>
#include "Kernels.h"
using namespace Rcpp;

// This is the exported Rcpp function that can be called from R
RcppExport SEXP cudaMandelbrot(SEXP resolution, SEXP iteration)
{
  // Convert parameters from SEXP objects
  int wh = Rcpp::as<int>(resolution);
  int iter = Rcpp::as<int>(iteration);
  
  std::vector<int> res = mandelbrot_main(wh, iter);

  // Wrap the resultant vector to a R object and return it
  return Rcpp::wrap(res);
}
