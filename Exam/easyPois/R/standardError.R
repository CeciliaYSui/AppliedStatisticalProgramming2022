#' Calculating the standard error
#'
#' Calculates the standard error. 
#' 
#'
#' @param y The vector of observed data.
#' @param SEtype The method used to calculate the standard error. It can take on two values: basic and bootstrap.
#' @param B The number of bootstrapped resamplings. 
#'
#' @return The function outputs the standard error calculated via the chosen method.
#' 
#' 
#' 
#' @author Cecilia Y. Sui: \email{c.sui@@wustl.edu}
#' @seealso \code{\link[PoisMLE]{logLik}}, \code{\link[PoisMLE]{mle}}, \code{\link[PoisMLE]{standardError}}, \code{\link[PoisMLE]{estimatePois}}
#' @rdname standardError
#' @include standardError.R
#' @aliases standardError-method
#' 
#' @examples
#' 



# ------------------------------------------------------
# Generic: standardError
# ------------------------------------------------------
#' @export
setGeneric(name = "standardError",
           def = function(y, SEtype, B)
           {standardGeneric("standardError")}
)


# ------------------------------------------------------
# Method: standardError
# ------------------------------------------------------
setMethod(f = "standardError",
          definition = function(y, SEtype, B){
            
            # check for SEtype input
            if (! SEtype %in% c("basic", "bootstrap")){
              stop("Value Error: use either basic or bootstrap for calculating standard errors.")
            }
            
            # check for B type
            if (!is.numeric(B)){
              stop("Type Error: B must be of numeric type.")
            }
            
            # check for B value
            if (B <= 0){
              stop("Value Error: B (the number of bootstrapped resamplings) must be a positive number.")
            }
            
            # extract n
            n <- length(y)
            
            # basic 
            if (SEtype == "basic"){
              
              # evaluate standard error 
              return(sqrt(mle(y) / n))
            }
            
            # bootstrap
            if (SEtype == "bootstrap"){
              
              # sample B times with replacement of sample size n from y  
              samples <- replicate(B, {(sample(y, n, replace = TRUE))})
              
              # calculate MLE for each sample using the mle() method created in the package
              mles <- apply(samples, 2, mle)
              
              # return standard deviation of the results above
              return(sd(mles))
              
            }
          }
)
