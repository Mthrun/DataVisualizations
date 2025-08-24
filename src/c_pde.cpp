#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::NumericVector c_pde(Rcpp::NumericVector kernels,int nKernels,double paretoRadius, Rcpp::NumericVector DataPlus) {
// c_pde(kernels, nKernels,paretoRadius,DataPlus)
// counts data in pareto radius around kernels
//INPUT
// kernels			[1:k] kernels defined in paretodensity estimation function
// nKernels			scalar k
// paretoRadius		scalar of paretoradius
// DataPlus			data [1:n] plus mirrored data on boundaries, see paretodensity estimation function
//OUTPUT
// paretoDensity     not normalized number of datapoints for each kernel counted in the pareto radius around the kernel

	//author: Michael Thrun
  //int n=DataPlus.size();
  //NumericVector isInParetoSphere(n);
  //double isInParetoSphere;
  
  Rcpp::NumericVector paretoDensity(nKernels);
  double lb;
  double ub;
  for(int i=0;i<nKernels;i++){
    lb = kernels[i] - paretoRadius;
    ub = kernels[i] + paretoRadius;
    //isInParetoSphere=0;
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