// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
arma::mat computeW(arma::mat X, std::string weightType, double sigma=0.3) {
  arma::mat W = arma::zeros<arma::mat>(X.n_cols, X.n_cols);
  if(weightType == "heat-kernel") {
    for(int i=0; i<W.n_rows; i++) {
      for(int j=0; j<W.n_cols; j++) {
	W(i, j)  = exp(pow(arma::norm((X.col(i)- X.col(j))), 2)/sigma);
      }
    }
  }
  else if(weightType == "dot-weighting") {
    for(int i=0; i<W.n_rows; i++) {
      for(int j=0; j<W.n_cols; j++) {
	W(i, j) = arma::dot(X.col(i), X.col(j));
      }
    }
  }
  return W;
}
