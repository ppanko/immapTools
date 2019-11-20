#' Find a relative "elbow" in a eigenvalue or R^2 vector
#'
#' @param .v a numeric vector containing eigenvalues or R-squared values.
#' @param cumulative a boolean indicating whether or not \code{r2} is cumulative.
#' @param plotD2 a boolean indicating whether or not the second derivative should be plotted
#' @param ... additional parameters to be passed to \code{plot}.
#' 
#' @details The function attempts to find the biggest relative
#'     drop-off in variance for a set of principal components using a
#'     vector of eigenvalues or individual R-squared statistics for
#'     the principal components. In addition, this function plots a
#'     horizontal line representing the average contribution of the
#'     vector \code{.v}.
#'  
#'
#' @seealso \code{graphics::plot}
#' 
#' @examples
#' ## Example r-squared vector
#' r2Vec <- c(.2, .1, .65, .43, .21, .1, .07)
#'
#' ## Plot efficiency diagnostics
#' findRelativeElbow(r2Vec)
#' @export

## Main "elbow" plotting function
findRelativeElbow <- function(.v, cumulative = FALSE, plotD2 = FALSE, ...) {
    ## Find individual contributions if the vector is cumulative
    if (cumulative) {
        .v <- c(.v[1], diff(.v))
    }
    ## Find a vector of second derivatives
    x           <- seq_along(.v)
    midPoints   <- findMid(x)
    secondDeriv <- derivative(midPoints, derivative(x, .v))
    ## Find all possible elbows
    possibleElbows <- vector("integer", length(.v))
    for (i in seq_along(secondDeriv)) {
        ## Look at all values starting from the current value of "i"
        currentDerivs     <- secondDeriv[i:length(secondDeriv)]
        ## Find the biggest second derivative
        possibleElbows[i] <- which(secondDeriv == max(currentDerivs)[1])
    }
    ## Find the biggest difference between the largest adjacent second
    ## derivatives
    elbowPoints   <- diff(possibleElbows)
    relativeElbow <- possibleElbows[which(elbowPoints == max(elbowPoints))]
    ## Find the average contribution
    center      <- abs(.v - mean(.v))
    meanContrib <- min(which(center == min(center)))
    ## Plot the values of .v, as well as the relative elbow and the
    ## average contribution
    graphics::plot(.v, ...)
    graphics::abline(v = relativeElbow, col = "red")
    graphics::abline(h = .v[meanContrib], col = "blue")
    graphics::mtext(relativeElbow, side = 1, at = relativeElbow, col = "red")
    ## Plot the second derivative, if necessaray
    if (plotD2) {
        d2Model <- stats::loess(secondDeriv ~ findMid(midPoints - 1))
        graphics::points(d2Model, col = "blue", pch = 18)
    }
    ## Return diagnostic message
    msg <- strwrap(
        x     = sprintf("Biggest drop in variance contribution happens at\n component %i, below average contributions start after component %i",
                        relativeElbow, meanContrib),
        width = 81
    )
    return(msg)
}

## Convenience function for calculating derivatives
derivative <- function(x, y) {
    diff(y) / diff(x)
}

## Convenience function for calculating mid-points
findMid <- function(.v) {
    .v[-1] - diff(.v) / 2
}
