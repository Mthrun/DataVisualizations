#include <Rcpp.h>
#include <algorithm>
using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::NumericVector c_pde_fast(Rcpp::NumericVector kernels, int nKernels, 
                         double paretoRadius, NumericVector DataPlus) {
  
  //Trick through prior sorting voids the repeated full-vector scans (which are O(n) per kernel) and instead does O(log(n)) work per kernel.
  
  // Clone and sort DataPlus once
  Rcpp::NumericVector sortedData = clone(DataPlus);
  std::sort(sortedData.begin(), sortedData.end());
  
  Rcpp::NumericVector paretoDensity(nKernels);
  
  for (int i = 0; i < nKernels; i++) {
    double lb = kernels[i] - paretoRadius;
    double ub = kernels[i] + paretoRadius;
    
    // Find the first element not less than lb
    auto lower = std::lower_bound(sortedData.begin(), sortedData.end(), lb);
    // Find the first element greater than ub
    auto upper = std::upper_bound(sortedData.begin(), sortedData.end(), ub);
    
    // The number of elements in [lb, ub] is the difference between the two iterators.
    paretoDensity[i] = upper - lower;
  }
  
  return paretoDensity;
}
