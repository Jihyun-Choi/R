##################################
## Lecture Material             ##
## for Probability & Statistics ##
## Minseok Seo                  ##
## 2021-10-13                   ##
##################################

setRepositories(ind = 1:7)

#install.packages("devtools")
#install.packages("ggVennDiagram")
#install.packages("animation")
#install.packages("scatterplot3d")

library(devtools)
#install_github('jhk0530/Rstat')

library(Rstat)
library(ggVennDiagram)
library(animation)
library(dplyr)
library(scatterplot3d)

# 지금까지 하며 모든 패키지를 깔았다. 혹시라도 오류뜨면 깔고 진행하면 된다.

######################################################
## Lab code for Chap.2                      ##
######################################################

## Self-Checking 6
x <- 1:6
f <- rep(1,6)
disc.exp(x, f)

#곱해지는 값이 1이 아닌 1/6이라는 것 유의!

## Self-Checking 7
x <- 1:6
f <- rep(1,6)
y <- 100*x - 400
disc.exp(y, f)

#기댓값과 분산의 차이점, 수식의 차이 유의!


######################################################
## Lab code for Chap.3                      ##
######################################################


## Self-Checking 1
x <- 1:20
p <- rep(1, 20)
disc.exp(x, p)

sum(x>=15)/length(x)


## Self-Checking 2
n <- 10
p <-c(0.2, 0.5, 0.8)

x <- 0:n


fx1 <- list()

for (i in 1:3){
  fx1[[i]] <- dbinom(x, n, p[i])
}

sapply(fx1, sum)

mt1 <- paste0("B(10,", p, ")")
disc.mexp(x, fx1, mt=mt1)


## Self-Checking 3
x <- 0:20
fx <- dbinom(x, 20, 0.03)

disc.exp(x, fx, prt=TRUE)

# Get Probability of finding 0, 1 or 2 defective products
dbinom(0:2, 20, 0.03)

# Get Probability of finding 3 or more defective products (two methods)
1-sum(dbinom(0:2, 20, 0.03))
pbinom(2, 20, 0.03, lower=F)




## Self-Checking 4
N <- 50
S <- c(10, 25, 40)
n <- 10
x <- 0:n


fx2 <- list()

for (i in 1:3){
  fx2[[i]] <- dhyper(x, S[i], N-S[i], n)
}

sapply(fx2, sum)

mt2 <- paste0("HG(10, 50,", S,")")
disc.mexp(x, fx2, mt=mt2)

mt12 = paste0("HG(10,50,", S,"):B(10,", p,")")
disc.mexp(x, fx2, fx1, mt=mt12)



## Self-Checking 5
x <- 0:20
fx <- dhyper(x, 50, 950, 30)

disc.exp(x, fx, prt=TRUE)

dhyper(0:3, 50, 950, 30)

sum(dhyper(0:3, 50, 950, 30))
phyper(3, 50, 950, 30)


## Self-Checking 6
L <- c(2, 5, 8)
x <- 0:30

fx3 = list()

for (i in 1:3){
  fx3[[i]] <- dpois(x, L[i])
}

sapply(fx3, sum)

mt3 <- paste0("Poisson(", L,")")
disc.mexp(x, fx3, mt=mt3)


mt123 <- paste0("HG(50) : B(10) : Pois (", L, ")")
x <- 0:10
for (i in 1:3){
  fx3[[i]] <- dpois(x, L[i])
}

disc.mexp(x, fx2, fx1, fx3, mt=mt123)


## Self-Checking 7
x <- 0:100
fx <- dpois(x, 1.5)
disc.exp(x, fx, prt=TRUE)

1-sum(dpois(0:2, 1.5))
ppois(2, 1.5, lower=F)


## Self-Checking 8
p <- c(0.1, 0.3, 0.5)
x <- 1:200

fx4 <- list()

for (i in 1:3){
  fx4[[i]] <- dgeom(x-1, p[i])
}

disc.mexp(x, fx4, plot=F)

mt4 <- paste0("Geometric(", p,")")
win.graph(9, 3)
par(mfrow=c(1,3))
par(mar=c(3,4,4,2))

for (k in 1:3){
  plot(1:50, fx4[[k]][1:50], type="h", main=mt4[k], ylab="f(x)", xlab="", lwd=3, col=2)
}


## Self-Checking 9
x <- 1:100
fx <- dgeom(x-1, 1/6)

disc.exp(x, fx, prt=TRUE)

sum(dgeom(0:2, 1/6))
pgeom(2, 1/6)


## Self-Checking 10
ps <- 0.4
r <- c(1, 2, 4)
xr <- 1:100

fx5 <- list()

for (i in 1:3){
  fx5[[i]] = dnbinom(xr-r[i], r[i], ps)
}

sapply(fx5, sum)

mt5 <- paste0("Neg-Binom(0.4,", r,")")
win.graph(9,3); par(mfrow=c(1,3)); par(mar=c(3,4,4,2))

for (k in 1:3){
  plot(xr[1:30], fx5[[k]][1:30], type="h", main=mt5[k], ylab="f(x)", xlab="", lwd=3, col=2)
}

disc.mexp(xr, fx5, plot=F)


## Self-Checking 11
x <- 3:250
ps <-0.1
r <- 3

fx = dnbinom(x-r, r, ps); disc.exp(x, fx)

dnbinom(10-r, r, ps)
