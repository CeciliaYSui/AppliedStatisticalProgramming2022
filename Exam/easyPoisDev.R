# ------------------------------------------------------
# Load libraries and set working directory
# ------------------------------------------------------
library(devtools)
library(roxygen2)

# ------------------------------------------------------
# Set working directory
# ------------------------------------------------------
setwd("/Users/ysui/Documents/GitHub/AppliedStatisticalProgramming2022/Exam")


# ------------------------------------------------------
# load package easyPois
# ------------------------------------------------------
current.code <- as.package("easyPois") 
# Load all of the functions so you can use them
load_all(current.code) 
document(current.code) # Make the help files


# ------------------------------------------------------
# Test functions 
# ------------------------------------------------------






