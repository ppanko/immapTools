#' Facilitate multiply-imputed data for use in Mplus 
#'
#' @param dataList list of data.frame objects.
#' @param dataDir character string of the file path for exported data.
#' @param miDir character string of the name of the directory where the imputed files will be written.
#' @param datPrefix character string of the name-prefix of the imputed files.
#' @param listName character string of the text file listing the names of the imputed files. 
#' @param colName character string of the file containing the column names of the imputed data.
#' @param missing character string of the missing data code.
#' @param ... additional parameters to be passed to \code{write.table}. 
#'
#' @seealso \code{write.table} for a list of options to pass via \code{...}.
#'
#' @examples
#' ## Create example data
#' data(iris)
#' irisList <- rep(list(iris), 10) 
#'
#' ## Basic usage
#' \dontrun{                              
#'   writeToMplus(                       
#'     dataList = irisList,              
#'     dataDir  = "my/example/directory",
#'     miDir    = "miFiles"              
#'   )                                   
#' }                                     
#' @export 

##
writeToMplus <- function(dataList, dataDir, miDir,
                         datPrefix = "data_",
                         listName  = "dataList.txt",
                         colName  = "dataNames.txt",
                         missing   = "-9999",
                         ...)
{
  dir.create(file.path(dataDir, miDir))
  fileNames <- vector("character", length(dataList))
  ##
  for(i in seq_along(dataList)) {
    ##
    fileNames[i] <- file.path(miDir, paste0(datPrefix, i, ".dat"))
    ##
    utils::write.table(
      x    = dataList[[i]],
      file = file.path(dataDir, fileNames[i]),
      row.names = FALSE,
      col.names = FALSE,
      quote = FALSE,
      na = missing,
      ...
    )
    cat("Wrote", i, "\n")
  }
  ##
  utils::write.table(
    x    = fileNames,
    file = file.path(dataDir, listName),
    row.names = FALSE,
    col.names = FALSE,
    quote = FALSE
  )
  ##
  utils::write.table(
    x    = names(dataList[[1]]),
    file = file.path(dataDir, colName),
    row.names = FALSE,
    col.names = FALSE,
    quote = FALSE
  )
}
