#include <Rcpp.h>
#include <RcppParallel.h>

using namespace RcppParallel;
using namespace Rcpp;
using namespace std;

// [[Rcpp::depends(RcppParallel)]]
struct PDEKernel : public Worker{
  const RVector<double> DataVector, DomainX;
  double PR;
  int NSeq, NData;
  RVector<double> MyPDEDensity;
  
  PDEKernel(const NumericVector DataVector,
            const NumericVector DomainX,
            double PR, int NSeq, int NData,
            NumericVector MyPDEDensity):
    DataVector(DataVector),
    DomainX(DomainX),
    PR(PR), NSeq(NSeq), NData(NData),
    MyPDEDensity(MyPDEDensity) {}
  
  // function call operator that work for the specified range (begin/end)
  void operator()(std::size_t begin, std::size_t end) {
    for(std::size_t i = begin; i < end; i++){
      int Counter = 0;
      for(int j = 0; j < NData; j++){
        if(abs(DomainX[i] - DataVector[j]) <= PR){
          Counter = Counter + 1;
        }
      }
      MyPDEDensity[i] = Counter;
    }
  }
};

// [[Rcpp::depends(RcppParallel)]]
// [[Rcpp::export]]
NumericVector PDE_Kernel(NumericVector DataVector, NumericVector DomainX, double PR, int NSeq, int NData){
  // 
  // Author: Quirin Stier 2024
  NumericVector MyPDEDensity(NSeq);
  PDEKernel PDEdensity(DataVector, DomainX, PR, NSeq, NData, MyPDEDensity);
  parallelFor(0, NSeq, PDEdensity);
  //DataBotsPos = internal_PDE_kernel(DistanceMatrix, MyPDEDensity, PR, NSeq, NData);
  return MyPDEDensity;
}
