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
# Load all of the functions so you can use them
load_all(current.code, quiet = TRUE) # please don't tell me all the warnings 
document(current.code, quiet = TRUE) # Make the help files


# ------------------------------------------------------
# Test functions 
# ------------------------------------------------------
y <- 1L:20L
estimatePois(y, "basic", lambda = 2)
estimatePois(y, "bootstrap", B = 1000, lambda = 2)






