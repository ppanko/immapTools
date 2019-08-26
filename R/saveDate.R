#' Get current date 
#'
#' @param format character string representing the structure of the returned date string.
#' 
#' @return a character string indicating the current date. Also prints the return to screen as a comment.
#' 
#' @seealso \code{format.Date} and \code{Sys.date}.
#' 
#' @examples
#' ## Standard usage 
#' saveDate()
#' ## Change format
#' saveDate(format = "%Y-%m-%d")
#' @export 

saveDate <- function(format = "%Y%m%d") {
    date <- format.Date(Sys.Date(), format)
    cat("#", date, "\n")
    return(date)
}
