#' Plot PCA R-squared efficiency diagnostics 
#'
#' @param r2 a numeric vector of R-squared values for each PC.
#' @param cumulative a boolean indicating whether or not \code{r2} is cumulative.
#' @param ... additional parameters to be passed to \code{plot}.
#' 
#' @details The function attempts to find the biggest drop-off in variance explained for a set of principal
#' components using a vector R-squared statistics for the principal components.  
#'
#' @seealso \code{graphics::plot}
#' 
#' @examples
#' ## Example r-squared vector
#' r2Vec <- c(.2, .1, .65, .43, .21, .1, .07)
#'
#' ## Plot efficiency diagnostics
#' r2Plot(r2Vec)
#' @export

## Main r-squared plotting function
r2Plot <- function(r2, cumulative = TRUE, ...) {
    ## Calculate non-cumulative r-squared
    if(cumulative)
        r2 <- c(r2[1], diff(r2))
    ## Calculate second derivative
    x <- seq_along(r2)
    d2 <- derivative(x[-1] - diff(x)/2, derivative(x, r2))
    ## Biggest drop-off
    elbows <- sapply(seq_along(d2), function(x) which(d2 == max(d2[x:length(d2)]))[1])
    drop   <- elbows[which(diff(elbows) == max(diff(elbows)))]
    ## Average efficiency
    cntr <- abs(r2 - mean(r2))
    slide <- min(which(cntr == min(cntr)))
    ## Create plot
    graphics::plot(r2, ...)
    graphics::abline(v = drop, col = "red")
    graphics::abline(h = r2[slide], col = "blue")
    graphics::mtext(drop, side = 1, at = drop, col = "red")
    ## Print report
    msg <- strwrap(sprintf("Biggest drop in variance contribution happens at
           component %i, below average contributions start after component %i", drop, slide),
           width = 81)
    return(msg)
}

## Convenience function for calculating derivatives 
derivative <- function(x, y) diff(y)/diff(x)
