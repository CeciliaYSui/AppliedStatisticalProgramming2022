#' Calculating log likelihood
#'
#' Calculates a log likelihood for the observed data based on Poisson distribution. 
#'
#'
#' @param y The vector of observed data.
#' @param lambda The assumed value of \eqn{\lambda}.
#'
#'
#' @return The function produces as an output: the log likelihood for the observed data conditioned on the value of \eqn{\lambda}.
#' 
#' 
#' 
#' @author Cecilia Y. Sui: \email{c.sui@@wustl.edu}
#' @seealso \code{\link[PoisMLE]{logLik}}, \code{\link[PoisMLE]{mle}}, \code{\link[PoisMLE]{standardError}}, \code{\link[PoisMLE]{estimatePois}}
#' @rdname logLik
#' @include logLik.R
#' @aliases logLik-method
#' 
#' @examples



# ------------------------------------------------------
# Generic: logLik
# ------------------------------------------------------
#' @export
setGeneric(name = "logLik",
           def = function(y, lambda)
           {standardGeneric("logLik")}
)


# ------------------------------------------------------
# Method: logLik
# ------------------------------------------------------
#' @export
setMethod(f = "logLik",
          definition = function(y, lambda){
            
            # check for valid lambda input 
            if (lambda <= 0) {
              stop("Lambda must be in the range of zero (exclusive) to infinity.")
            }
            
            # extract n: the number of observations 
            n <- length(y)
            
            # evaluate log likelihood 
            LL <- -n * lambda - sum(log(factorial(y))) + log(lambda) * sum(y)

            return(LL)
          }
)
