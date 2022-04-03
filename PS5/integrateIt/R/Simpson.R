#' Simpson Integration Object 
#' 
#' Object of class \code{Simpson} are created by the \code{integrateIt} and \code{print} functions
#'
#' 
#' An object of the class `Simpson' has the following slots:
#' \itemize{
#' \item \code{x} {a vector of values}
#' \item \code{y} {a vector of evaluated values}
#' \item \code{integral} {the numerical approximation of the definite integral}
#' }
#'
#' @author Cecilia Sui: \email{c.sui@@wustl.edu}
#' @aliases Simpson-initialize, integrateIt-method
#' @rdname Simpson

check_simpson <- function(object){
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
  
  # compute integral using the Simpson Rule Formula given 
  result <- (h / 3) * (f_a + sum(4 * y_vals[c(T, F)]) + sum(2 * y_vals[c(F,T)]) + f_b)
  
  # check integral 
  if (object@integral != result){
    errors <- c(errors, "Integration is not valid.")
  }
  
  if (length(errors) == 0) TRUE else errors 
}




#' @export
setClass(Class = "Simpson", 
         representation = representation(
           x = "numeric",
           y = "numeric",
           integral = "numeric"
         ),
         prototype = prototype(
           x = c(),
           y = c(),
           integral = c()
         ),
         validity = check_simpson
)



#' @export
setMethod("initialize", 
          "Simpson", 
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
          (x = "Simpson"),
          function(x){ 
            print(x@integral)
          }
)











