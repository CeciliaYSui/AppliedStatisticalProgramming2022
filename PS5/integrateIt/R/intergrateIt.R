#' Integrating functions 
#'
#' Integrates function via Trapezoidal and Simpson's Rule
#'
#'
#' @param x_in A vector of input values 
#' @param f_x A function to generate evaluated values y based on input x_in
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
#' integrateIt(seq(1, 10, 0.1), function(x){x+1}, c(2,5), rule = "Simpson")
#' 
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


# ------------------------------------------------------
# Method: integrateIt
# ------------------------------------------------------
#' @export
setMethod(f = "integrateIt",
          definition = function(x_in, f_x, range, rule){
            
            # check for valid rule input 
            if (! rule %in% c("Simpson", "Trapezoid")){
              error("Invalid Rule. Please use Simpson or Trapezoid.")
            }
            
            # extract values needed for integration
            x_vals <- x_in[x_in >= range[1] & x_in <= range[2]]
            y_vals <- unlist(lapply(x_vals, f_x))
            n <- length(x_vals)
            h <- (range[2] - range[1]) / n

            # evaluate integral with Trapezoidal rule 
            if (rule == "Trapezoid") {
              result <- (h / 2) * (y_vals[1] + sum(2 * y_vals[2:(n-1)]) + y_vals[n])
              return(new("Trapezoid", 
                         x = x_vals, 
                         y = y_vals,
                         integral = result))
            }
            
            # evaluate integral with Simpson's rule
            if (rule == "Simpson") {
              y_vals_mid <- y_vals[2:(n-1)]
              result <- (h / 3) * (y_vals[1] + sum(4 * y_vals_mid[c(T, F)]) + sum(2 * y_vals_mid[c(F,T)]) + y_vals[n])
              return(new("Simpson", 
                         x = x_vals, 
                         y = y_vals,
                         integral = result))
            }
          }
)

