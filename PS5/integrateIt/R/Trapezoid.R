#' Trapezoid Integration Object 
#' 
#' Object of class \code{Trapezoid} are created by the \code{integrateIt} and \code{print} functions
#'
#' 
#' An object of the class `Trapezoid' has the following slots:
#' \itemize{
#' \item \code{integral} the numerical approximation of the definite integral
#' \item \code{x} a vector of values
#' \item \code{y} a vector of evaluated values
#' }
#'
#' @author Cecilia Sui: \email{c.sui@@wustl.edu}
#' 
#' @aliases Trapezoid initialize, integrateIt-method
#' 
#' @rdname Trapezoid
#' 
#' @export
setClass(Class = "Trapezoid", 
         representation = representation(
             integral = "numeric",
             x = "numeric",
             y = "numeric"
         ),
         prototype = prototype(
             integral = c(),
             x = c(),
             y = c()
         ),
         validity = check_trapezoid
)



# ------------------------------------------------------
# Initializer
# ------------------------------------------------------
#' @export
setMethod("initialize", 
          "Trapezoid", 
          function(.Object, ...){
            value = callNextMethod()
            validObject(value)
            return(value)
          }
) 




# ------------------------------------------------------
# Method: print
# ------------------------------------------------------
#' @export
setMethod("print", 
          (x = "Trapezoid"),
          function(x){ 
            print(x@integral)
          }
)



# ------------------------------------------------------
# validity check for Trapezoid objects 
# ------------------------------------------------------
check_trapezoid <- function(object){
  # create errors log 
  errors <- character()
  
  # check NA for x
  if (any(is.na(object@x))){
    errors <- c(errors, "Vector x contains NA values.")
  }
  
  # check NA for y
  if (any(is.na(object@y))){
    errors <- c(errors, "Vector y contains NA values.")
  }
  
  # check integration result
  # extract values needed 
  a <- object@x[1]
  b <- tail(object@x, 1)
  f_a <- object@y[1]
  f_b <- tail(object@y, 1)
  n <- length(object@x)
  x_vals <- object@x[a < object@x & b > object@x]
  y_vals <- object@y[f_a < object@y & f_b > object@y]
  
  # compute values needed for integration 
  h <- (b - a) / n 
  
  # compute integral using the Trapezoidal Rule Formula given 
  result <- (h / 2) * (f_a + sum(2 * y_vals) + f_b)
  
  # check integral 
  if (object@integral != result){
    errors <- c(errors, "Integration is not valid.")
  }

  if (length(errors) == 0) TRUE else errors 
}









