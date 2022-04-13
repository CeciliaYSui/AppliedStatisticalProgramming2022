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
#' @author Cecilia Y. Sui: \email{c.sui@@wustl.edu}
#' @seealso \code{\link[PoisMLE]{logLik}}, \code{\link[PoisMLE]{mle}}, \code{\link[PoisMLE]{standardError}}, \code{\link[PoisMLE]{estimatePois}}
#' @rdname PoisMLE
#' @include PoisMLE.R
#' @aliases PoisMLE-class
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
  
  # check MLE data type
  if (!is.numeric(object@MLE)){
    errors <- c(errors, "The MLE (maximum likelihood estimator) must be of numeric type.")
  }
  
  # check LL data type
  if (!is.numeric(object@LL)){
    errors <- c(errors, "The LL (log likelihood) must be of numeric type.")
  }
  
  # check SE data type
  if (!is.numeric(object@SE)){
    errors <- c(errors, "The LL (log likelihood) must be of numeric type.")
  }
  
  # check SEtype data type
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

