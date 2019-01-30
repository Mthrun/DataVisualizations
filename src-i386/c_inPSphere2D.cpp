#include <stdlib.h> 
#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace std;
using namespace sugar;


// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
IntegerVector c_inPSphere2D(NumericMatrix data, IntegerVector xBinNr, IntegerVector yBinNr, unsigned int nrXBins, unsigned int nrYBins, unsigned int nrData, double paretoRadius){
  /* 
  *   Anmerkung:
  *   Diese Implementation verh?lt sich anders als die in R.
  *   Da diese hier aber, im Gegensatz zur R implementation,
  *   sich so verh?lt wie die urspr?ngliche implementation in
  *   Matlab, wurde das einfach so gelassen.
  *   
  *   Das Problem ist, in R treten manchmal 1en auf, wo
  *   sie nicht hingeh?ren.
  */
  
  // This will hold the result
  std::vector<int> output;
  output.resize(nrData,0);
  
  for(int i=0; i < int(nrXBins); i++){
    for(int j=0; j < int(nrYBins); j++){
      std::vector<int> pointsInCenterTileInd, pointsInSurroundingInd;
      for(unsigned int k=0; k < nrData; k++){
        if(xBinNr(k) == i && yBinNr(k) == j){
          pointsInCenterTileInd.push_back(k);
        } else if((abs(xBinNr(k) - i) < 2) && (abs(yBinNr(k) - j) < 2)) {
          pointsInSurroundingInd.push_back(k);
        }
      }
      int nrInCenterTile = pointsInCenterTileInd.size();
      if(nrInCenterTile > 0){
        std::vector<int> points;
        points.insert(points.end(), pointsInCenterTileInd.begin(), pointsInCenterTileInd.end());
        points.insert(points.end(), pointsInSurroundingInd.begin(), pointsInSurroundingInd.end());
        uint32_t numrows = points.size();
        uint32_t numcols = data.ncol();
        // Matrix erzeugen ?ber der Distanzen berechnet werden.
        arma::mat matrixfordist = arma::mat(numrows,numcols);
        for(uint32_t row = 0; row < numrows; row++){
          int datarow = points[row];
          for(unsigned int col = 0; col < numcols; col++){
            matrixfordist(row,col) = data(datarow,col);
          }
        }
        
        // Distanzen:
        // (angelehnt an fastPdistC)
        arma::colvec mdn =  sum(square(matrixfordist),1);
        arma::mat C = -2 * (matrixfordist * matrixfordist.t());
        C.each_col() += mdn;
        C.each_row() += mdn.t();
        arma::mat dists = sqrt(C);

        // distanceCenter2All9
        // dists.resize(nrInCenterTile,size(dists)[1]);
        arma::mat distanceCenter2All9 = dists.rows(0, nrInCenterTile - 1);
//         std::cout << std::endl;
//         distanceCenter2All9.print();
        
        // Compare
        arma::mat bools(distanceCenter2All9.n_rows,distanceCenter2All9.n_cols,arma::fill::zeros);
        bools.elem(find(distanceCenter2All9 > paretoRadius)).zeros();  
        bools.elem(find(distanceCenter2All9 <= paretoRadius)).ones();
        
        arma::colvec colsums = arma::sum(bools,1);
        //colsums.resize(nrInCenterTile,1);
        for(int ind = 0; ind < nrInCenterTile; ind++ ){
          output[pointsInCenterTileInd[ind]] = colsums[ind];
        }
        
      }
    }
  }

  return(wrap(output));
}

