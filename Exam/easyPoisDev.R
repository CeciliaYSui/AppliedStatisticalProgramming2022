# ------------------------------------------------------
# Load libraries and set working directory
# ------------------------------------------------------
library(devtools)
library(roxygen2)

# ------------------------------------------------------
# Set working directory
# ------------------------------------------------------
# setwd("/Users/ysui/Documents/GitHub/AppliedStatisticalProgramming2022/Exam")


# ------------------------------------------------------
# load package easyPois
# ------------------------------------------------------
current.code <- as.package("easyPois") 

# Load all of the functions
load_all(current.code, quiet = TRUE) # please don't tell me all the warnings :)

# Make the help files
document(current.code, quiet = TRUE) 


# ------------------------------------------------------
# Testing some functions 
# ------------------------------------------------------
set.seed(1023)
y <- sample(1L:20L, 100, replace = TRUE)
n <- length(y)
lambda <- 2
estimatePois(y, "basic", lambda = 2)
estimatePois(y, "bootstrap", B = 1000, lambda = 2)
logLik(y, lambda = 2)
mle(y)
standardError(y, "bootstrap", B = 1000)


