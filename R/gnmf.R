#' Compute Weight Matrix
#'
#' @description Computes a weight matrix for the input matrix provided using method
#' requested.
#'
#' @usage computeWeight(coil20$X, weightType="heat-kernel", sigma=0.5)
#'
#' @param input a matrix of input data.
#'
#' @param weightType the method by which the weight matrix is computed.
#'
#' @param sigma parameter used in heat-kernel weight matrix calculation, default 0.3.
#'
#' @return A matrix of weights
#'
#' @export
#'
#' @examples
#' coil20  <- readMat("./testData/COIL20.mat")
#' W  <- computeWeight(coil20$X, weightType="dot-weighting")
#'
computeWeight  <- function(input, weightType=c("heat-kernel", "dot-weighting"),
                           sigma=0.3) {
    weightType  <- match.arg(weightType)
    if(sigma <= 0 && weightType=="heat-kernel") {
        stop(paste0("Sigma value supplied was equal to or less than zero ",
                    "and heat kernel weighting was selected.  Please choose a ",
                    "different method or a positive value of sigma"))
    }
    computeW(input, weightType, sigma)
}

#' GNMF based on Cai et al's implementation
#' @description implemention of GNMF in R and Rcpp that is based on Cai et al's
#' implementation in Matlab.
#'
#' @usage gnmfCai(input, nClust, weightMatrix, alpha, minIter, normalizeW, maxIter=NA,
#'                diffError=1e-5, nRepeat=10, meanFitRatio=0.1)
#'
#' @param input a matrix of input data, must be positive.
#'
#' @param nClust desired number of features to reduce to
#'
#' @param weightMatrix weight matrix returned by computeWeight
#'
#' @param alpha parameter that scales W.  IF this parameter is 0 or less this function
#' returns NMF.
#'
#' @param minIter minimum number of iterations to complete
#'
#' @param normalizeW boolean variable indicating whether or not W should be normalized
#'
#' @param maxIter optional parameter, specifies the maximum number of iterations
#'
#' @param diffError optional parameter, used as threshold against which error is
#' measured
#'
#' @param nRepeat number of times to repeat the optimization, defaults to 10
#'
#' @param meanFitRatio scales mean fit, defaults to 0.1
#'
#' @export
#'
#' @examples
#' uv <- gnmfCai(coil20$X, 8, W, alpha=1, minIter=5, normalizeW=TRUE)
#'
gnmfCai  <- function(input, nClust, weightMatrix, alpha, minIter, normalizeW, maxIter=NA,
                     diffError=1e-5, nRepeat=10, meanFitRatio=0.1) {
    if(any(input < 0)) {
        stop("Input must be non-negative")
    }
    if(is.na(maxIter)) {
        maxIter = -505
    }
    gnmf(input, nClust, weightMatrix, maxIter, alpha, normalizeW, inIter, diffError,
         nRepeat, FALSE, meanFitRatio)
    
}
                     
