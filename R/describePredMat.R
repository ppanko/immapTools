#' Briefly describe a predictor matrix
#'
#' @param predMat a numeric matrix.
#' @param usePcNames a scalar boolean value indicating whether or not to include a report about principal components.
#'
#' @return a list containing information about the names and numbers of predictors. 
#'
#' @details The function splits the predictor matrix by categories: overall, incomplete variables, complete variables
#' and principal components. The number or predictors and the names of the predictors are then withdrawn for each variable
#' in each category and are returned to the user.
#'
#' @examples
#' data(iris)                                                      
#' irisPredMat <- matrix(0, ncol = ncol(iris), nrow = ncol(iris))  
#' dimnames(irisPredMat) <- list(names(iris), names(iris))         
#' irisPredMat[1, 2] <- 1                                          
#' irisPredMat[3, 1] <- 1                                          
#' diag(irisPredMat) <- 1                                          
#'                                                                 
#' ## Describe example predictor matrix                            
#' describePredMat(                                                
#'   predMat = irisPredMat                                   
#' )                                                               
#' 
#' @export

##
describePredMat <- function(predMat, usePcNames = FALSE) {
  ##
  if(usePcNames){
    pcNames      <- grep("inPC", colnames(predMat), value = TRUE)
  }
  completeVars <- predMat[ ,apply(predMat, 2, function(x) all(!is.na(x)))]
  compNames    <- names(completeVars)
  incNames     <- setdiff(colnames(predMat), compNames)
  ##
  predMatList <- list(
    regular    = predMat,
    incomplete = predMat[ , incNames],
    complete   = predMat[ , compNames]
  )
  if(usePcNames) {
    predMatList$pcs <- predMat[ , pcNames]
    remove <- c(compNames, pcNames)
  } else {
    remove <- compNames
  }
  ##
  lapply(predMatList, getPredInfo, remove)
}

removeZeroes <- function(.v) .v[.v != 0]

getPredInfo <- function(.m, remove = "") {
  .m      <- .m[setdiff(rownames(.m), remove), ]
  outList <- stats::setNames(vector("list", nrow(.m)), rownames(.m))
  for(i in seq_along(outList)) {
    .v           <- .m[i,]
    outList[[i]] <- list(
      numberPredictors = removeZeroes(sum(.v)),
      predictorNames   = names(removeZeroes(.v))
    )
  }
  outList
}
