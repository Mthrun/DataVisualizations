#include <Rcpp.h>
#include <algorithm>
#include <cmath>
using namespace Rcpp;

// Count pairs with distance <= d in sorted x
long long countPairsLE(const std::vector<double>& x, double d) {
  long long cnt = 0;
  int n = x.size();
  int j = 0;
  for (int i = 0; i < n; i++) {
    while (j < n && x[j] - x[i] <= d) j++;
    cnt += (j - i - 1); // pairs (i, i+1...j-1)
  }
  return cnt;
}

// [[Rcpp::export]]
double quantileDist1d(NumericVector x, double p = 0.18) {
  int n = x.size();
  if (n < 2) return NA_REAL;
  
  long long m = (long long)n * (n - 1) / 2;
  long long k = (long long) std::floor(p * m);
  
  std::vector<double> data(x.begin(), x.end());
  std::sort(data.begin(), data.end());
  
  // Binary search bounds
  double lo = 0.0;
  double hi = data.back() - data.front();
  double ans = hi;
  
  while (hi - lo > 1e-9 * (1 + hi)) { // relative tolerance
    double mid = 0.5 * (lo + hi);
    long long cnt = countPairsLE(data, mid);
    if (cnt > k) {
      ans = mid;
      hi = mid;
    } else {
      lo = mid;
    }
  }
  
  return ans;
}
