#include <Rcpp.h>
#include <algorithm>
#include <cmath>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector quantile4LargeVectors(NumericVector x, NumericVector probs) {
  int n = x.size();
  int np = probs.size();
  NumericVector out(np);
  
  // For a single quantile, use nth_element (average O(n))
  if(np == 1) {
    double p = probs[0];
    int idx = std::min(n - 1, std::max(0, static_cast<int>(std::floor(n * (p - 1e-9)))));
    NumericVector y = clone(x); // clone because nth_element modifies in place
    std::nth_element(y.begin(), y.begin() + idx, y.end());
    out[0] = y[idx];
  } else {
    // If multiple quantiles, sort the vector once (O(n log n))
    NumericVector y = clone(x);
    std::sort(y.begin(), y.end());
    for(int i = 0; i < np; i++){
      int idx = std::min(n - 1, std::max(0, static_cast<int>(std::floor(n * (probs[i] - 1e-9)))));
      out[i] = y[idx];
    }
  }
  
  return out;
}

//using namespace std;
//Rcpp::NumericVector quantile4LargeVectors_old(Rcpp::NumericVector x, Rcpp::NumericVector probs) {
//  NumericVector y = clone(x);
//  std::sort(y.begin(), y.end());
//  return y[x.size()*(probs - 0.000000001)];
//}