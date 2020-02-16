#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector c_pde(NumericVector kernels,int nKernels,double paretoRadius, NumericVector DataPlus) {
  //int n=DataPlus.size();
  //NumericVector isInParetoSphere(n);
  double isInParetoSphere;
  NumericVector paretoDensity(nKernels);
  double lb;
  double ub;
  for(int i=0;i<nKernels;i++){
    lb = kernels[i] - paretoRadius;
    ub = kernels[i] + paretoRadius;
    isInParetoSphere=0;
    //using cpp lambda function in count_if 
    paretoDensity[i] = std::count_if(DataPlus.begin(), DataPlus.end(), [lb, ub](double i) { return (i >= lb && i <=ub) ; });
    //for(int j=0;j<n;j++){
    //  isInParetoSphere= isInParetoSphere+(DataPlus[j] >= lb) && (DataPlus[j] <= ub);
    //}
    //paretoDensity[i] = sum(isInParetoSphere);
   // paretoDensity[i] = isInParetoSphere;
  }
  return paretoDensity;
}