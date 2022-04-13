#' Define PoisMLE Class
#' 
#' 
#' An object of class \code{PoisMLE} can be created by the \code{estimatePois()} function
#'
#' 
#' An object of class `PoisMLE` has the following slots:
#' (All descriptions are based on the exam project description file.)
#' \itemize{
#' \item \code{y} The original data
#' \item \code{MLE} The maximum likelihood estimator for this dataset
#' \item \code{LL} The log likelihood calculated from the observed data assuming the MLE is correct
#' \item \code{SE} The standard error for the MLE
#' \item \code{SEtype} The method used to calculate the standard error 
#' }
#' 
#' 
#' The \code{\link[PoisMLE]{estimatePois}} function calculates several statistics 
#' related to Poisson distribution based on a vector of observed data, including the log likelihood, 
#' MLE estimate, and its associated standard error. The output is an Object of class \code{PoisMLE}.
#' One can use the \code{plot()} function to produce a simple visualization of the MLE estimate with 95\% confidence interval. 
#'
#' @author Cecilia Y. Sui: \email{c.sui@@wustl.edu}
#' @seealso \code{\link[PoisMLE]{logLik}}, \code{\link[PoisMLE]{mle}}, \code{\link[PoisMLE]{standardError}}, \code{\link[PoisMLE]{estimatePois}}
#' @rdname PoisMLE
#' @include PoisMLE.R
#' @aliases PoisMLE-class
#' 
#' 


# ------------------------------------------------------
# Function: validity check for PoisMLE objects
# ------------------------------------------------------
check_PoisMLE <- function(object){
  # create errors log 
  errors <- character()
  
  # check NA for y
  if (any(is.na(object@y))){
    errors <- c(errors, "Vector y contains NA values.")
  }
  
  # check datatype for y
  if (!is.integer(object@y)){
    errors <- c(errors, "Vector y must be of integer type.")
  }
  
  # check non-negative values for y
  if (any(object@y < 0)){
    errors <- c(errors, "Vector y must only contain positive integers or zero.")
  }
  
  # check MLE 
  if (!is.numeric(object@MLE)){
    errors <- c(errors, "The MLE (maximum likelihood estimator) must be of numeric type.")
  }
  
  # check LL
  if (!is.numeric(object@LL)){
    errors <- c(errors, "The LL (log likelihood) must be of numeric type.")
  }
  
  # check SE 
  if (!is.numeric(object@SE)){
    errors <- c(errors, "The LL (log likelihood) must be of numeric type.")
  }
  
  # check SEtype
  if (!is.character(object@SEtype)){
    errors <- c(errors, "The SEtype (log likelihood) must be a character string.")
  }
  
  # if there are errors, return errors 
  if (length(errors) == 0) TRUE else errors
}


# ------------------------------------------------------
# Class: PoisMLE
# ------------------------------------------------------
#' @export
setClass(Class="PoisMLE",
         representation = representation(
           y = "integer",
           MLE = "numeric",
           LL = "numeric",
           SE = "numeric",
           SEtype = "character"
         ),
         prototype = prototype(
           y = c(),
           MLE = c(),
           LL = c(),
           SE = c(),
           SEtype = character()
         ),
         validity = check_PoisMLE
)


# ------------------------------------------------------
# Initializer
# ------------------------------------------------------
#' @export
setMethod("initialize", 
          "PoisMLE", 
          function(.Object, ...){
            value = callNextMethod()
            validObject(value)
            return(value)
          }
) 


# ------------------------------------------------------
# Method: plot
# ------------------------------------------------------
setMethod("plot", 
          c(x = "PoisMLE", y = "missing"), 
          function(x, ...){
            upper <- x@MLE + 1.96 * x@SE
            lower <- x@MLE - 1.96 * x@SE
            plot(x@MLE, 
                 xlab = "", 
                 ylab = "MLE", 
                 main = "MLE")
          }
) 
