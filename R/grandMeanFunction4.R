#' Create a "grand mean" data set from a series of imputed data sets  
#'
#' @param stackedData a data.frame of several stacked imputed data sets
#' @param idName a character vector of length 1 providing the name of the imputation id variable. 
#' @param contNames a character vector providing the names of the continuous variables. Default is NULL.
#' @param discNames a character vector providing the names of the discrete variables. Default is NULL.
#' @param dropNames a character vector providing the names of the variables to be excluded from the operation. Default is NULL.
#'
#' @details This function is an extension of the \code{aggregate} function for imputed data sets that contain multiple variable types. The default behavior is to aggregate the continuous variables inferred from \code{contNames} using the arithmetic mean, and to aggregate the discrete variables inferred from \code{discNames} using mode, while the variables inferred from \code{dropNames} are removed.   
#' 
#' @return a data.frame with the number of columns equal to \code{stackedData} and the number of rows equal to the unique values in the id column. 
#' 
#' @seealso \code{aggregate}
#' 
#' @examples
#' ## Create example data
#' data(iris)
#' iris$id <- factor(1:nrow(iris))
#' irisStack <- do.call(rbind, rep(list(iris), 10))
#' irisStack$junk <- sample(1:2000, nrow(iris))
#'
#' ## Create name vectors
#' irisContNames <- names(which(sapply(iris, is.numeric)))
#' irisDiscNames <- "Species"
#' irisDropNames <- "junk"
#' irisIdName <- "id"
#' 
#' ## Create grand mean
#' gm <- grandMean(              
#'   stackedData = irisStack,    
#'   idName      = irisIdName,   
#'   contNames   = irisContNames,
#'   discNames   = irisDiscNames,
#'   dropNames   = irisDropNames 
#' )                             
#' @export 

## 
grandMean <- function(stackedData,
                      idName, 
                      contNames   = NULL,
                      discNames   = NULL,
                      dropNames   = NULL) {
  ##
  if(any(is.na(stackedData[[idName]]))) {
    stop("NA values are not allowed in the id")
  }
  ##
  typeList <- list(
    contNames = contNames,
    discNames = discNames
  )
  ##
  funList <- list(
    contNames = doMean,
    discNames = doMode
  )
  ##
  nameVec  <- unlist(c(unname(typeList), idName, dropNames))
  facNames <- which(sapply(stackedData, is.factor))

  ## 
  if(any(duplicated(nameVec))) {
    stop("Duplicated names found")
  }
  if(!all(nameVec %in% names(stackedData))) {
    stop("Not all provided names exist in data") 
  }
  if(!all(sapply(stackedData[contNames], is.numeric))) {
    stop("Some of the variables in 'contNames' are non-numeric")
  }
  ##
  if(any(facNames)) {
    stackedData[facNames] <- lapply(stackedData[facNames], as.character)
  }
  ## 
  stackedData <- stackedData[setdiff(names(stackedData), dropNames)]
  ##
  doGrandMean(
    .df         = stackedData,
    idName      = idName,
    varTypeList = typeList,
    aggFunList  = funList
  )
}

## 
doGrandMean <- function(.df, idName, varTypeList, aggFunList) {
  ## 
  varNameVec <- unlist(varTypeList)
  idVec      <- .df[[idName]]
  aggLen     <- length(unique(idVec))
  ##
  outDf        <- data.frame(
    matrix(nrow = aggLen, ncol = ncol(.df)),
    stringsAsFactors = FALSE
  )
  names(outDf) <- names(.df)
  ##
  for(varName in varNameVec) {
    aggFun           <- aggFunList[[getType(varName, varTypeList)]]
    outDf[[varName]] <- unlist(tapply(.df[[varName]], idVec, aggFun))
    browser()
  }
  ##
  outDf[[idName]] <- unlist(tapply(idVec, idVec, doMode))
  outDf
}

##
getType <- function(varName, lookUpList) {
  typeVec <- sapply(lookUpList, function(.v) any(.v == varName))
  names(which(typeVec))
}

## 
doMode <- function(x) {
  ##
  tab  <- table(x, exclude = NULL)
  mode <- names(tab)[tab == max(tab, na.rm = TRUE)]
  ##
  if(length(mode) > 1) {
    mode <- sample(mode, 1)
  }
  ##
  mode
}

##
doMean <- function(x) {
  mean(x, na.rm = TRUE)
}
