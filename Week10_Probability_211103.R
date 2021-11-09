##################################
## Lecture Material             ##
## for Probability & Statistics ##
## Minseok Seo                  ##
## 2021-10-20                   ##
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


######################################################
## Lab code for Chap.3                      ##
######################################################



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



######################################################
## Lab code for Chap.4                      ##
######################################################


## Self-Checking 1
mu <- c(0, 0, 2, 2)
sd <- c(1, 2, 1, 2)

getpdf <- function(dist, xa, para, para2) {
  np = length(xa)
  N = max(length(para), length(para2))
  
  # PDF name
  dpdf <- paste0("d", dist)
  pdf <- matrix(NA, nrow=np, ncol=N)
  
  # Vector of the PDF
  if (dist %in% c("exp", "t", "chisq")) { 
    for (k in 1:N) {
      pdf[, k] <- do.call(dpdf, list(xa, para[k]))
    }
  } else if (dist == "gamma") { 	
    for (k in 1:N) {
      pdf[, k] <- do.call(dpdf, list(xa, para[k], 1/para2[k]))
    }
  } else { 	for (k in 1:N) {
    pdf[, k] <- do.call(dpdf, list(xa, para[k], para2[k]))
  }
  }
  invisible(pdf)
}

cont.spdf("norm", -7, 7, mu, sd, xp=mu)


## Self-Checking 2
pnorm(1) - pnorm(-1.5)
norm.trans(mu = 2, sig = 2, a = -1, b = 4)


## Self-Checking 3
pv <- matrix(pnorm(0:299/100), ncol=10, byrow=T)
colnames(pv) <- 0:9/100
rownames(pv) <- 0:29/10
print(round(pv, 4))


## Self-Checking 4
pnorm(185, 175, 8) - pnorm(180, 175, 8)


## Self-Checking 5
pbinom(45, 100, 0.5)-pbinom(39, 100, 0.5)

pnorm(-0.9)-pnorm(-2.1)


## Self-Checking 6
nu <- c(5, 10, 15, 20)
up <- qchisq(0.99, max(nu))

cont.spdf("chi", 0, up, para=nu, xp=nu)



## Self-Checking 7
nu <- c(1, 5, 10, 30)
tnorm.comp(nu)


## Self-Checking 8

# Define function for CLT check
plotForCLT <- function(r.dist, n, ...) {
  
  means <- c()
  
  for(i in 1:1000){
    means[i] <- mean(r.dist(n, ...))
  } 
  
  std.means <- scale(means)
  par(mfrow = c(1, 2))
  
  hist(std.means, prob = T, col = "light grey",
       border = "grey", main = NULL, ylim = c(0, 0.5))
  lines(density(std.means))
  box()
  
  curve(dnorm(x, 0, 1), -3, 3, col = 'blue', add = T)
  
  qqnorm(std.means, main="", cex = 0.8)
  abline(0, 1, lty = 2, col = "red")
  par(mfrow = c(1, 1))
}


# Sampling from the Chi-squared distribution
plotForCLT(rchisq, n = 1, df = 1)
plotForCLT(rchisq, n = 10, df = 1)
plotForCLT(rchisq, n = 20, df = 1)
plotForCLT(rchisq, n = 30, df = 1)
plotForCLT(rchisq, n = 100, df = 1)

# Sampling from the Binomial distribution
plotForCLT(rbinom, n = 1, size = 1, p = .5)
plotForCLT(rbinom, n = 10, size = 1, p = .5)
plotForCLT(rbinom, n = 20, size = 1, p = .5)
plotForCLT(rbinom, n = 30, size = 1, p = .5)
plotForCLT(rbinom, n = 50, size = 1, p = .5)
plotForCLT(rbinom, n = 100, size = 1, p = .5)

# Sampling from the Poisson distribution
plotForCLT(rpois, n = 1, lambda = 1)
plotForCLT(rpois, n = 10, lambda = 1)
plotForCLT(rpois, n = 20, lambda = 1)
plotForCLT(rpois, n = 30, lambda = 1)
plotForCLT(rpois, n = 40, lambda = 1)

# Sampling from the Negative Binomial distribution
plotForCLT(rnbinom, n = 1, size = 1, p = .5)
plotForCLT(rnbinom, n = 10, size = 1, p = .5)
plotForCLT(rnbinom, n = 20, size = 1, p = .5)
plotForCLT(rnbinom, n = 30, size = 1, p = .5)




