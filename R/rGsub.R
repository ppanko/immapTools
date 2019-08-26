#' Replace a vector of matches using \code{gsub} recursively 
#'
#' @param pattern character vector containing the pattern(s) to be matched.
#' @param replacement character vector containing the replacement(s) for the matched patterns.
#' @param x character vector in which the pattern(s) are to be replaced. 
#' @param ... additional parameters to be passed to \code{gsub}.
#' 
#' @return character vector equal in length to the vector provided via argument \code{x}. 
#' 
#' @seealso \code{grep} for possible arguments to pass via \code{...}.
#' 
#' @examples
#' ## Many-to-many replacement 
#' patterns <- c("a", "b", "c")
#' replacements <- c(1, 2, 3)  
#' rGsub(                      
#'   pattern     = patterns,   
#'   replacement = replacements,
#'   x           = letters     
#' )                           
#'                             
#' ## Many-to-one replacement  
#' replacement <- 123          
#' rGsub(                      
#'   pattern     = patterns,   
#'   replacement = replacement,
#'   x           = letters     
#' )                           
#'                             
#' ## One-to-one replacement   
#' pattern <- "a"              
#' rGsub(                      
#'   pattern     = pattern,    
#'   replacement = replacement,
#'   x           = letters     
#' )
#' 
#' @export 

##
rGsub <- function(pattern, replacement, x, ...) {
    ## Initialize local variables for pattern/replacement length
    .pl <- length(pattern)
    .rl <- length(replacement)
    ## If more replacements than patterns, break
    if(.pl < .rl) {
        stop("More replacements than patterns")
        ## If length of replacements and patterns are not equal, break
    } else if (.rl > 1 && .rl != .pl) {
        stop("Patterns and replacements don't line up")
    }
    ## Recursive one-to-one replacement
    if(.pl > 1 && .rl > 1) {
        x <- gsub(pattern[[1]], replacement[[1]], x, ...)
        rGsub(pattern[-1], replacement[-1], x, ...)
        ## Recursive many-to-one replacement
    } else if(.pl > 1 && .rl == 1) {
        x <- gsub(pattern[[1]], replacement, x, ...)
        rGsub(pattern[-1], replacement, x, ...)
        ## Standard replacement
    } else if(.pl == 1) {
        x <- gsub(pattern, replacement, x, ...)
        return(x)
    }
}
