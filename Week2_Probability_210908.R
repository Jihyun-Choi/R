 
setRepositories(ind = 1:7)

# 한번 설치 시 다시 설치할 필요 없음
install.packages("devtools")
library(devtools)

install.packages("devtools")
install_github('jhk0530/Rstat')
install.packages("ggVennDiagram")
install.packages("animation")

library(devtools)
library(Rstat)
library(ggVennDiagram)
library(animation)
library(dplyr)


## Set two fair dice (Self-Checking 1)
nsides <- 6
times <- 2

temp <- list()

for (i in 1:times) {
  temp[[i]] <- 1:nsides
}

## Sample Space S
S <- expand.grid(temp, KEEP.OUT.ATTRS = FALSE)
names(S) <- c(paste(rep("Dice_", times), 1:times, sep = ""))
nrow(S)


