##################################
## Lecture Material             ##
## for Probability & Statistics ##
## Minseok Seo                  ##
## 2021-10-06                   ##
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
## Lab code for Chap.2                      ##
######################################################


## Self-Checking 1
S <- tosscoin2(3)

countT <- function(x){
  sum(x == "T")
}

X <- apply(S, 1, countT)

pX <- table(X) / nrow(S)
pX
plot(pX)


## Self-Checking 2
nTimes <- 4

S <- rolldie2(nTimes)

N <- nrow(S)
X <- apply(S, 1, sum)

X.freq <- table(X)
print(addmargins(X.freq))

X.prob <- X.freq/N
print(round(addmargins(X.prob), 4))

X.val <- as.numeric(names(X.freq))

EX <- sum(X.val * X.prob)
EX2 <- sum(X.val^2 * X.prob)
VX <- EX2 - EX^2
DX <- sqrt(VX)

Xmin <- min(X.val)
Xmax <- max(X.val)

win.graph(7, 5)
plot(X.prob, type = "h", col = "red", main = paste0("Probability Distribution of the Sum of ", nTimes, " Dice"), lwd = 4, ylim = c(0, max(X.prob) + 0.01))

fitnorm <- function(x) dnorm(x, EX, DX)
curve(fitnorm, Xmin, Xmax, add = T, col = 4)
text(Xmin:Xmax, X.prob, labels = X.freq, pos = 3, col = 4, 
     cex = 1)
legend("topright", c(paste("S-S.Size =", N), paste("E(X) =", EX), paste("D(X) =", round(DX, 4))), bg = "white")



## Self-Checking 3
xv <- 0:3
xp <- choose(3, 0:3)

xname <- deparse(substitute(xv))
if (sum(xp) > 1) {
  xp <- xp/sum(xp)
}

xcdf <- c(0, cumsum(xp))
sf <- stepfun(xv, xcdf)

plot(sf, verticals = F, pch = 19, lwd = 2, cex = 1.2, col = 2, xlab = "x", ylab = "F(x)")
grid(col = 3)
points(xv, xcdf[-length(xcdf)], col = 2, cex = 1)
text(xv, xcdf[-1], labels = round(xcdf[-1], 3), cex = 1, 
     col = 4, pos = 2)
EX <- sum(xv * xp)
EX2 <- sum(xv^2 * xp)
VX <- EX2 - EX^2
DX <- sqrt(VX)
legend("bottomright", c(paste("E(X) =", round(EX, 4)), paste("D(X) =", round(DX, 4))), bg = "white")



## Self-Checking 4
S <- rolldie2(2)
N <- nrow(S)

X <- apply(S, 1, max)
Y <- apply(S, 1, min)

disc.joint2(X, Y, plot = T)



## Self-Checking 5
X <- c(-3, -1, 1, 3)*100
P <- c(1, 3, 3, 1)/8
disc.exp(X, P, plot = T)


## Self-Checking 6
x <- 1:6
f <- rep(1,6)
disc.exp(x, f)


## Self-Checking 7
x <- 1:6
f <- rep(1,6)
y <- 100*x - 400
disc.exp(y, f)


######################################################
## Lab code for Chap.3                      ##
######################################################


## Self-Checking 1
x <- 1:20
p <- rep(1, 20)
disc.exp(x, p)

sum(x>=15)/length(x)






# pdf 
dbinom(x, size, prob)
?dbinom

# cdf 
pbinom(q, size, prob, lower.tail = TRUE)
?pbinom

# Bionomial Random Variable
rbinom(n, size, prob)
?rbinom



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




