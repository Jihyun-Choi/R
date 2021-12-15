##################################
## HomeWork 3                   ##
## for Probability & Statistics ##
## Jihyun Choi                  ##
## 2021-12-14                   ##
##################################

setRepositories(ind = 1:7)

#install.packages("data.table")
library(data.table)

######################################################
## Homework 2                                       ##
######################################################

WORK_DIR <- "C:\\Users\\user\\HW3"
setwd(WORK_DIR)
getwd()

Data <- data.frame(fread("Data1.txt", sep = "\t", head=T, stringsAsFactors = T))
class(Data)
dim(Data)
str(Data) 

Gender <- Data$Gender 

Age <- Data$Age 
Height <- Data$Height_CM
Weight <- Data$Weight_KG	 
sysBP <- Data$sysBP	
HR <- Data$HR
Resting <- Data$Resting_SaO2
BMI <- Data$BMI	
FEV1pp <- Data$FEV1pp_utah	
FVCpp <- Data$FVCpp_utah	
FEV1_FVC <- Data$FEV1_FVC_utah
 

var(Gender)
var(Age)
var(Height)
var(Weight)
var(sysBP)
var(HR)
var(Resting)
var(BMI)
var(FEV1pp)
var(FVCpp)
var(FEV1_FVC)
 


####################################################################
#t.test(~, Data, alternative = "two.sided")$p.value  # 모수적 방법 - 양측검정
#t.test(~, Data, alternative = "less")$p.value       # 모수적 방법 - 단측검정
#t.test(~, Data, alternative = "greater")$p.value    # 모수적 방법 - 단측검정
#wilcox.test(~, Data, exact = FALSE)$p.value         # 비모수적 방법

## Age
Data <- data.frame(Age, Gender) 
t.test(Age~Gender, Data, alternative = "two.sided")$p.value 
t.test(Age~Gender, Data, alternative = "less")$p.value 
t.test(Age~Gender, Data, alternative = "greater")$p.value 
wilcox.test(Age ~ Gender, Data, exact = FALSE)$p.value 


## Height_CM	
Data <- data.frame(Height, Gender) 
t.test(Height~Gender, Data, alternative = "two.sided")$p.value 
t.test(Height~Gender, Data, alternative = "less")$p.value 
t.test(Height~Gender, Data, alternative = "greater")$p.value 
wilcox.test(Height~Gender, Data, exact = FALSE)$p.value 

## Weight_KG	
Data <- data.frame(Weight, Gender) 
t.test(Weight~Gender, Data, alternative = "two.sided")$p.value 
t.test(Weight~Gender, Data, alternative = "less")$p.value 
t.test(Weight~Gender, Data, alternative = "greater")$p.value 
wilcox.test(Weight~Gender, Data, exact = FALSE)$p.value 

## sysBP	
Data <- data.frame(sysBP, Gender) 
t.test(sysBP~Gender, Data, alternative = "two.sided")$p.value 
t.test(sysBP~Gender, Data, alternative = "less")$p.value 
t.test(sysBP~Gender, Data, alternative = "greater")$p.value 
wilcox.test(sysBP~Gender, Data, exact = FALSE)$p.value 

## HR	
Data <- data.frame(HR, Gender) 
t.test(HR~Gender, Data, alternative = "two.sided")$p.value 
t.test(HR~Gender, Data, alternative = "less")$p.value 
t.test(HR~Gender, Data, alternative = "greater")$p.value 
wilcox.test(HR~Gender, Data, exact = FALSE)$p.value 


## Resting_SaO2	
Data <- data.frame(Resting, Gender) 
t.test(Resting~Gender, Data, alternative = "two.sided")$p.value 
t.test(Resting~Gender, Data, alternative = "less")$p.value 
t.test(Resting~Gender, Data, alternative = "greater")$p.value 
wilcox.test(Resting~Gender, Data, exact = FALSE)$p.value 

## BMI	
Data <- data.frame(BMI, Gender) 
t.test(BMI~Gender, Data, alternative = "two.sided")$p.value 
t.test(BMI~Gender, Data, alternative = "less")$p.value 
t.test(BMI~Gender, Data, alternative = "greater")$p.value 
wilcox.test(BMI~Gender, Data, exact = FALSE)$p.value 

## FEV1pp_utah		
Data <- data.frame(FEV1pp, Gender) 
t.test(FEV1pp~Gender, Data, alternative = "two.sided")$p.value 
t.test(FEV1pp~Gender, Data, alternative = "less")$p.value 
t.test(FEV1pp~Gender, Data, alternative = "greater")$p.value 
wilcox.test(FEV1pp~Gender, Data, exact = FALSE)$p.value 

## FVCpp_utah			
Data <- data.frame(FVCpp, Gender) 
t.test(FVCpp~Gender, Data, alternative = "two.sided")$p.value 
t.test(FVCpp~Gender, Data, alternative = "less")$p.value 
t.test(FVCpp~Gender, Data, alternative = "greater")$p.value 
wilcox.test(FVCpp~Gender, Data, exact = FALSE)$p.value 

## FEV1_FVC_utah		
Data <- data.frame(FEV1_FVC, Gender) 
t.test(FEV1_FVC~Gender, Data, alternative = "two.sided")$p.value 
t.test(FEV1_FVC~Gender, Data, alternative = "less")$p.value 
t.test(FEV1_FVC~Gender, Data, alternative = "greater")$p.value 
wilcox.test(FEV1_FVC~Gender, Data, exact = FALSE)$p.value 

 

