#include <Rcpp.h>

using namespace Rcpp;


//' For all elements of a vector a, find the closest elements in a vector B and returns resulting indexes
//' @param a numeric vector, ordered
//' @param b numeric vector, ordered
//' @param twosided logical value, if false, indexes will always point to elements in b that are less than or equal to elements in a but not greater than.
//' @return integer vector of the same size as a with elements represnting indexes pointing to closest values in b
// [[Rcpp::export(name = ".bfast_cpp_closestfrom")]]
IntegerVector bfast_cpp_closestfrom(const NumericVector& a, const NumericVector& b, const bool twosided) {
  int na = a.size();
  int nb = b.size();
  IntegerVector out(na);
  
  if (na <= 0 || nb <= 0) {
    stop("Input vectors must have length > 0");
  }
  
  int j=1;
  for (int i = 0; i < na; i++) {
    while(j < (nb-1) && a[i] >= b[j]) ++j; 
   
    // compare b[j] and b[j-1]
    if (!twosided) {
      out[i] = j;
    }
    else {
      if (fabs(a[i] - b[j]) < fabs(a[i]-b[j-1])) {
        out[i] = j+1; // one based indexes in R
      }
      else {
        out[i] = j; // one based indexes in R
      }
    }
  }
  return out;
}
