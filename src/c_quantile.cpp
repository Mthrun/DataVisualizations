#include <Rcpp.h>
//https://github.com/RcppCore/Rcpp/issues/967
//author Dirk eddelbuettel
// [[Rcpp::export]]
Rcpp::NumericVector c_quantile(Rcpp::NumericVector x, Rcpp::NumericVector probs) {
//out=c_quantile(data, 0.5)
// fast implementation of type 7 quantile of R
//INPUT
// datavector [1:n] numerical vector
// probs scalar or vector of relevant quantiles
//Output: 
// fast quantiles of numerical vector
  const size_t n=x.size(), np=probs.size();
  if (n==0) return x;
  if (np==0) return probs;
  Rcpp::NumericVector index = (n-1.)*probs, y=x.sort(), x_hi(np), qs(np);
  Rcpp::NumericVector lo = Rcpp::floor(index), hi = Rcpp::ceiling(index);
  
  for (size_t i=0; i<np; ++i) {
    qs[i] = y[lo[i]];
    x_hi[i] = y[hi[i]];
    if ((index[i]>lo[i]) && (x_hi[i] != qs[i])) {
      double h;
      h = index[i]-lo[i];
      qs[i] = (1.-h)*qs[i] + h*x_hi[i];
    }
  }
  return qs;
}