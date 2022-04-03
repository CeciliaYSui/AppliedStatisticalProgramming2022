# ------------------------------------------------------
# Load libraries and set working directory
# ------------------------------------------------------
library(devtools)
library(roxygen2)


# ------------------------------------------------------
# Set working directory
# ------------------------------------------------------
setwd("/Users/ysui/Documents/GitHub/AppliedStatisticalProgramming2022/PS5")


# ------------------------------------------------------
# load package 
# ------------------------------------------------------
current.code <- as.package("integrateIt") 
# Load all of the functions so you can use them
load_all(current.code) 
document(current.code) # Make the help files


# ------------------------------------------------------
# Test functions 
# ------------------------------------------------------
test1 <- integrateIt(1:10, function(x){x+1}, c(2,5), rule = "Simpson")
print(test1)

# test Trapezoid
integrateIt(1:10, function(x){x+1}, c(2,5), rule = "Trapezoid")
# more accurate with smaller intervals 
integrateIt(seq(1, 10, 0.1), function(x){x+1}, c(2,5), rule = "Simpson")


# check(current.code) # Run the R checks and will have lots of warnings here. lol 
