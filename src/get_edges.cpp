#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List get_edges(NumericMatrix AdjacencyMatrix, NumericVector x_vertex, NumericVector y_vertex) {
  int n = AdjacencyMatrix.nrow();
  
  std::vector<double> X1, Y1, X2, Y2;
  
  for (int i = 1; i < n; ++i) {
    for (int j = 0; j < i; ++j) {
      if (AdjacencyMatrix(i, j) > 0) {
        X1.push_back(x_vertex[i]);
        Y1.push_back(y_vertex[i]);
        X2.push_back(x_vertex[j]);
        Y2.push_back(y_vertex[j]);
      }
    }
  }
  
  return List::create(
    Named("X1") = X1,
    Named("Y1") = Y1,
    Named("X2") = X2,
    Named("Y2") = Y2
  );
}