#' Integrating functions 
#'
#' Integrates function via Trapezoidal and Simpson's Rule
#'
#'
#' @param x_in A vector of values
#' @param f_x A function to generate evaluated values y
#' @param range Starting and ending values 
#' @param rule Method of integration: Trapezoid or Simpson
#'
#'
#' @return An object of class Simpson or Trapezoid containing
#' \item \code{integral} {the numerical approximation of the definite integral}
#' \item \code{x} {a vector of values}
#' \item \code{y} {a vector of evaluated values based on some function}
#' 
#' @author Cecilia Y. Sui
#' @note This produces an object of a class Trapezoid or Simpson
#' @examples
#' 
#' integrateIt(1:10, function(x){x+1}, c(2,5), rule = "Trapezoid")
#' integrateIt(1:10, function(x){x+1}, c(2,5), rule = "Simpson")
#' 
#' @seealso \code{\link{Trapezoid}}, \code{\link{Simpson}}
#' @rdname integrateIt
#' @aliases integrateIt, ANY-method
#' 
#' @export
setGeneric(name = "integrateIt",
           def = function(x_in, f_x, range, rule)
           {standardGeneric("integrateIt")}
)



#' @export
setMethod(f = "integrateIt",
          definition = function(x_in, f_x, range, rule){
            if (! rule %in% c("Simpson", "Trapezoid")){
              error("Invalid Rule. Please use Simpson or Trapezoid.")
            }
            
            # extract values needed for integration
            a <- range[1]
            b <- range[2]
            f_a <- unlist(lapply(a, f_x))
            f_b <- unlist(lapply(b, f_x))
            x_vals <- x_in[x_in > a & x_in < b]
            y_vals <- unlist(lapply(x_vals, f_x))
            n <- length(x_vals) + 2
            h <- (b - a) / n 
            
            # evaluate integral
            if (rule == "Trapezoid") {
              result <- (h / 2) * (f_a + sum(2 * y_vals) + f_b)
              return(new("Trapezoid", 
                         x = c(a, x_vals, b), 
                         y = c(f_a, y_vals, f_b),
                         integral = result))
            }
            
            if (rule == "Simpson") {
              result <- (h / 3) * (f_a + sum(4 * y_vals[c(T, F)]) + sum(2 * y_vals[c(F,T)]) + f_b)
              return(new("Simpson", 
                         x = c(a, x_vals, b), 
                         y = c(f_a, y_vals, f_b),
                         integral = result))
            }
          }
)



