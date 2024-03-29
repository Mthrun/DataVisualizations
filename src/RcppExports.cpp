// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// c_pde
NumericVector c_pde(NumericVector kernels, int nKernels, double paretoRadius, NumericVector DataPlus);
RcppExport SEXP _DataVisualizations_c_pde(SEXP kernelsSEXP, SEXP nKernelsSEXP, SEXP paretoRadiusSEXP, SEXP DataPlusSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type kernels(kernelsSEXP);
    Rcpp::traits::input_parameter< int >::type nKernels(nKernelsSEXP);
    Rcpp::traits::input_parameter< double >::type paretoRadius(paretoRadiusSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type DataPlus(DataPlusSEXP);
    rcpp_result_gen = Rcpp::wrap(c_pde(kernels, nKernels, paretoRadius, DataPlus));
    return rcpp_result_gen;
END_RCPP
}
// c_quantile
Rcpp::NumericVector c_quantile(Rcpp::NumericVector x, Rcpp::NumericVector probs, int sorted);
RcppExport SEXP _DataVisualizations_c_quantile(SEXP xSEXP, SEXP probsSEXP, SEXP sortedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type probs(probsSEXP);
    Rcpp::traits::input_parameter< int >::type sorted(sortedSEXP);
    rcpp_result_gen = Rcpp::wrap(c_quantile(x, probs, sorted));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_DataVisualizations_c_pde", (DL_FUNC) &_DataVisualizations_c_pde, 4},
    {"_DataVisualizations_c_quantile", (DL_FUNC) &_DataVisualizations_c_quantile, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_DataVisualizations(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
