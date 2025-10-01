#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
Rcpp::NumericVector dist1d(Rcpp::NumericVector x) {
                      const int n = x.size();
                      if (n < 2) return Rcpp::NumericVector(0);
                      const long long m = (long long)n * (n - 1) / 2;
                      Rcpp::NumericVector out(m);
                      long long idx = 0;
                      for (int i = 0; i < n - 1; ++i) {
                        for (int j = i + 1; j < n; ++j) {
                          out[idx++] = std::abs(x[i] - x[j]);
                        }
                      }
                      return out;
                    }

