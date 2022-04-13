#' Estimating Poisson Distribution
#'
#' Estimates Poisson Distribution
#' 
#'
#' @param y The vector of observed data.
#' @param SEtype The method used to calculate the standard error. It can take on two values: basic and bootstrap.
#' @param B The number of bootstrapped resamplings. 
#' @param lambda The assumed value of \eqn{\lambda}.
#'
#' @return An object of class PoisMLE containing: 
#' \item \code{y} {The original data}
#' \item \code{MLE} {The maximum likelihood estimator for this dataset}
#' \item \code{LL} {The log likelihood calculated from the observed data assuming the MLE is correct}
#' \item \code{SE} {The standard error for the MLE}
#' \item \code{SEtype} {The method used to calculate the standard error}
#' 
#' 
#' 
#' @author Cecilia Y. Sui: \email{c.sui@@wustl.edu}
#' @seealso \code{\link[PoisMLE]{logLik}}, \code{\link[PoisMLE]{mle}}, \code{\link[PoisMLE]{standardError}}, \code{\link[PoisMLE]{estimatePois}}
#' @rdname estimatePois
#' @include estimatePois.R
#' @aliases estimatePois-method
#' 
#' @examples
#' 



# ------------------------------------------------------
# Generic: estimatePois
# ------------------------------------------------------
#' @export
setGeneric(name = "estimatePois",
           def = function(y, SEtype, B, lambda)
           {standardGeneric("estimatePois")}
)


# ------------------------------------------------------
# Method: estimatePois
# ------------------------------------------------------
#' @export
setMethod(f = "standardError",
          definition = function(y, SEtype, B, lambda){
            
            # return a PoisMLE object
            return(new("PoisMLE",
                       y = y,
                       MLE = mle(y),
                       LL = logLik(y, lambda),
                       SE = standardError(y, SEtype, B),
                       SEtype = SEtype))
          }
)


