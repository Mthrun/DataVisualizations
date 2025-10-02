#include <Rcpp.h>
#include <algorithm>
#include <cmath>
using namespace Rcpp;
// Count how many pairs (i < j) in sorted x satisfy (x[j] - x[i] <= d).
// x must be sorted ascending.
// Two-pointer technique: overall O(n).
long long countPairsLE(const std::vector<double>& x, double d) {
  long long cnt = 0; // total count of qualifying pairs
  int n = x.size();  // number of points
  int j = 0;  // right pointer
  for (int i = 0; i < n; i++) { // left pointer
    // Move j forward while the distance from x[i] to x[j] is <= d
    while (j < n && x[j] - x[i] <= d) j++;
    // For this i, indices (i+1) ... (j-1) form valid pairs with i
    cnt += (j - i - 1); // pairs (i, i+1...j-1)
  }
  return cnt; // total pairs with distance <= d
}

// [[Rcpp::export]]
double quantileDist1d(NumericVector x, double p = 0.18) {
  /*
   quantileDist1d
   
   Compute the p-quantile (e.g., p = 0.18) of all pairwise absolute
   distances |x_j - x_i| for a 1D numeric vector x, **without** materializing
   the O(n^2) distance vector.
   
   Inputs
   ------
   x : Rcpp::NumericVector
   A numeric vector of length n (finite values recommended). The function
   treats x as 1D coordinates and considers all pairs (i < j).
   p : double  (default = 0.18)
   Quantile in [0, 1]. For example, 0.18 returns the 18th percentile of
   the pairwise distances.
   
   Output
   ------
   double
   The p-quantile of the multiset { |x_j - x_i| : 1 <= i < j <= n }.
   Returns NA_real_ if n < 2.
   
   Complexity
   ----------
   - Sort step: O(n log n)
   - Each count (for a candidate distance) via two-pointer sweep: O(n)
   - Binary search iterations: ~30–40 for double precision
   Overall: O(n log n + n * log(range/eps)), **much** better than O(n^2).
   
   Notes
   -----
   - Exact result (not an approximation): we binary-search the threshold d
   such that the number of pairs with distance <= d hits the desired rank.
   - Assumes standard “type-7” style rank target using floor(p * m), where
   m = n*(n-1)/2 is the number of pairs.
   - If x contains NA/NaN/Inf, behavior depends on how std::sort orders them.
   Prefer to pre-filter or check your data.
   */
  int n = x.size();   // number of input points
  if (n < 2) return NA_REAL; //at least one pair required
  // Total number of pairs m = n*(n-1)/2 (use 64-bit to avoid overflow)
  long long m = (long long)n * (n - 1) / 2;
  // Target rank (0-based) for the p-quantile within the multiset of distances
  // Using floor(p * m) mirrors the usual quantile rank selection convention.
  long long k = (long long) std::floor(p * m);
  // Copy x into a std::vector<double> so we can sort it efficiently
  std::vector<double> data(x.begin(), x.end());
  // Sort coordinates ascending; all distances become simple forward differences
  std::sort(data.begin(), data.end());
  
  // Binary search bounds for the distance threshold:
  // smallest possible distance is 0, largest is range (max - min)
  double lo = 0.0;
  double hi = data.back() - data.front();
  // best-known answer so far
  double ans = hi;
  
  // Continue until relative interval is sufficiently small.
  // Using a relative tolerance: stop when (hi - lo) <= 1e-9 * (1 + hi).
  // This yields ~double-precision accuracy on typical data scales.
  while (hi - lo > 1e-9 * (1 + hi)) { // relative tolerance
    double mid = 0.5 * (lo + hi);  // candidate distance
    long long cnt = countPairsLE(data, mid);// pairs with distance <= mid
    // If we already have more than k pairs <= mid, the true quantile
    // is <= mid, so shrink the upper bound.
    if (cnt > k) {
      ans = mid;// record feasible upper bound
      hi = mid;   // tighten high side
    } else {
      // Otherwise, we need a larger distance to reach rank k
      lo = mid;// raise the low side
    }
  }
  
  return ans;// p-quantile distance
}
