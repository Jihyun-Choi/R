##################################
## HomeWork 2                   ##
## for Probability & Statistics ##
## Jihyun Choi                  ##
## 2021-12-02                   ##
##################################

setRepositories(ind = 1:7)

install.packages("data.table")
library(data.table)

######################################################
## Homework 2                                       ##
######################################################

WORK_DIR <- "C:\\Users\\user\\HW"
setwd(WORK_DIR)
getwd()


Data <- data.frame(fread("Data.txt", sep = "\t", head=T, stringAsFactors = T))
class(Data)
dim(Data)
str(Data)

double[][] = 
idx <- which(Data$Disease == "COVID19")
filteredData <- Data[idx,]

View(Data)

############################
z.test <- function(input, mu, var){
  z <- (mean(input) - mu) / (sqrt(var / length(input)))
  return(z)
}


2*pnorm(-abs(Zstat)) # two-sided Z-test











