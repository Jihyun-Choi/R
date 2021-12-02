##################################
## HomeWork 2                   ##
## for Probability & Statistics ##
## Jihyun Choi                  ##
## 2021-12-02                   ##
##################################

setRepositories(ind = 1:7)

#install.packages("data.table")
library(data.table)

######################################################
## Homework 2                                       ##
######################################################

WORK_DIR <- "C:\\Users\\user\\HW"
setwd(WORK_DIR)
getwd()

Data <- data.frame(fread("Data.txt", sep = "\t", head=T, stringsAsFactors = T))
class(Data)

dim(Data)
str(Data)

double[][] = 
  idx <- which(Data$Disease == "COVID19")
filteredData <- Data[idx,]

dim(filteredData)
nrow(filteredData)
View(filteredData)

x_bar = mean(filteredData$sysBP)
x_bar 
n = nrow(filteredData)
mu = 127.5
var = 16.5^2

###########################################################
z.test <- function(input, mu, var){
  z <- (mean(input) - mu) / (sqrt(var / length(input)))
  return(z)
}

# Get Z-statistic
Zstat <- z.test(filteredData$sysBP, mu=127.5, var=16.5^2)


# Q1
2*pnorm(-abs(Zstat)) # two-sided Z-test

# Q2
1-pnorm(Zstat) # one-sided Z-test (Z >= COVID19&sysBP)

# Q3
pnorm(Zstat) # one-sided Z-test (Z <= COVID19&sysBP)

