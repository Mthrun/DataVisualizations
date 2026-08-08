#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <vector>

using namespace Rcpp;

namespace {

inline int sign_of_difference(const double Value) {
  if (Value > 0.0) return 1;
  if (Value < 0.0) return -1;
  return 0;
}

Rcpp::List empty_result() {
  return Rcpp::List::create(
    Rcpp::Named("Amplitude") = 0.0,
    Rcpp::Named("MaximumIndices") = Rcpp::IntegerVector(0),
    Rcpp::Named("AntimodeIndex") = NA_INTEGER
  );
}

}  // namespace

// Scans a density grid for adjacent peaks and an intervening antimode. The
// density itself is estimated by stats::density() in R so that the established
// numerical definition and plotting behaviour are retained.
//
// [[Rcpp::export]]
Rcpp::List BimodalityAmplitudeCoreCpp(
    const Rcpp::NumericVector& DensityX,
    const Rcpp::NumericVector& DensityY) {
  const R_xlen_t NumberOfPoints = DensityY.size();

  if (DensityX.size() != NumberOfPoints) {
    Rcpp::stop("'DensityX' and 'DensityY' must have equal length.");
  }
  if (NumberOfPoints < 3) return empty_result();

  for (R_xlen_t Index = 0; Index < NumberOfPoints; ++Index) {
    if (!R_finite(DensityX[Index]) || !R_finite(DensityY[Index])) {
      Rcpp::stop("Density coordinates must contain only finite values.");
    }
  }

  std::vector<int> Maxima;
  std::vector<int> Minima;
  Maxima.reserve(static_cast<std::size_t>(NumberOfPoints / 4));
  Minima.reserve(static_cast<std::size_t>(NumberOfPoints / 4));

  // This reproduces diff(sign(diff(DensityY))) == -2 for maxima and == 2
  // for minima in the historical R implementation. A missing turning point
  // (historical comment: "Wendepunkt nicht auffindbar") returns zero.
  for (R_xlen_t Middle = 1; Middle < NumberOfPoints - 1; ++Middle) {
    const int PreviousSign = sign_of_difference(
      DensityY[Middle] - DensityY[Middle - 1]
    );
    const int NextSign = sign_of_difference(
      DensityY[Middle + 1] - DensityY[Middle]
    );
    const int Change = NextSign - PreviousSign;

    if (Change == -2) Maxima.push_back(static_cast<int>(Middle));
    if (Change == 2) Minima.push_back(static_cast<int>(Middle));
  }

  if (Maxima.size() < 2 || Minima.empty()) return empty_result();

  const int FirstMaximum = Maxima.front();
  const int LastMaximum = Maxima.back();
  double BestAmplitude = -R_PosInf;
  int BestLeft = -1;
  int BestRight = -1;
  int BestMinimum = -1;

  for (const int Minimum : Minima) {
    if (Minimum <= FirstMaximum || Minimum >= LastMaximum) continue;

    const std::vector<int>::const_iterator RightIterator =
      std::upper_bound(Maxima.begin(), Maxima.end(), Minimum);
    if (RightIterator == Maxima.begin() || RightIterator == Maxima.end()) {
      continue;
    }

    // Historical logic: select the greatest maximum index smaller than the
    // antimode and the smallest maximum index greater than the antimode.
    const int RightMaximum = *RightIterator;
    const int LeftMaximum = *(RightIterator - 1);
    const double LowerPeakHeight = std::min(
      DensityY[LeftMaximum],
      DensityY[RightMaximum]
    );
    const double AntimodeHeight = DensityY[Minimum];

    if (!(LowerPeakHeight > 0.0) || !R_finite(LowerPeakHeight) ||
        !R_finite(AntimodeHeight)) {
      continue;
    }

    const double Amplitude =
      (LowerPeakHeight - AntimodeHeight) / LowerPeakHeight;

    if (R_finite(Amplitude) && Amplitude > BestAmplitude) {
      BestAmplitude = Amplitude;
      BestLeft = LeftMaximum;
      BestRight = RightMaximum;
      BestMinimum = Minimum;
    }
  }

  if (BestMinimum < 0 || !R_finite(BestAmplitude)) {
    return empty_result();
  }

  BestAmplitude = std::max(0.0, std::min(1.0, BestAmplitude));

  return Rcpp::List::create(
    Rcpp::Named("Amplitude") = BestAmplitude,
    Rcpp::Named("MaximumIndices") = Rcpp::IntegerVector::create(
      BestLeft + 1,
      BestRight + 1
    ),
    Rcpp::Named("AntimodeIndex") = BestMinimum + 1
  );
}
