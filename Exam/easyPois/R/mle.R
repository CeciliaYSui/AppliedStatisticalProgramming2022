#' Calculating maximum likelihood estimator
#'
#' Calculates the maximum likelihood estimator for lambda. 
#' 
#'
#' @param y The vector of observed data.
#'
#'
#' @return The function outputs the maximum likelihood estimator for \eqn{\lambda}.
#' 
#' 
#' 
#' @author Cecilia Y. Sui: \email{c.sui@@wustl.edu}
#' @seealso \code{\link[PoisMLE]{logLik}}, \code{\link[PoisMLE]{mle}}, \code{\link[PoisMLE]{standardError}}, \code{\link[PoisMLE]{estimatePois}}
#' @rdname mle
#' @include mle.R
#' @aliases mle-method
#' 
#' @examples
#' 



# ------------------------------------------------------
# Generic: mle
# ------------------------------------------------------
#' @export
setGeneric(name = "mle",
           def = function(y)
           {standardGeneric("mle")}
)

# ------------------------------------------------------
# Method: mle
# ------------------------------------------------------
setMethod(f = "mle",
          definition = function(y){
            
            # evaluate MLE based on formula 
            return(sum(y)/length(y))
          }
)
