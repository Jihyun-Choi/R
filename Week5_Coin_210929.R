##################################
## Lecture Material             ##
## for Probability & Statistics ##
## Minseok Seo                  ##
## 2021-09-29                   ##
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
## Lab code for Chap.1 (Cont.)                     ##
######################################################

## Toss coin (Self-Checking 3)
times <- 4

temp <- list()
for (i in 1:times) {
  temp[[i]] <- c("H", "T")
}

S <- expand.grid(temp, KEEP.OUT.ATTRS = FALSE)
names(S) <- c(paste(rep("X", times), 1:times, sep = ""))

S <- S[order(S$X1, S$X2, S$X3, S$X4),]
element(S)

countHeadFunction <- function(x) sum(x == "H")

A <- subset(S, apply(S, 1, countHeadFunction) >= 2)
pprt(A, nrow(S))
nrow(A) / nrow(S)


## rolling four dices at the same time(Self-Checking 4)
S <- rolldie2(4)

A <- subset(S, X1+X2+X3+X4 >=15)
pprt(A, nrow(S))

B <- subset(S, apply(S, 1, max)==6)
pprt(B, nrow(S))

C <- subset(S, apply(S, 1, min)==1)
pprt(C, nrow(S))

AB <- intersect2(A,B)
AC <- intersect2(A,C)
BC <- intersect2(B,C)

ABC <- intersect2(AB,C) 

pprt(AB, nrow(S))
pprt(AC, nrow(S))
pprt(BC, nrow(S))
pprt(ABC, nrow(S))

# Drawing Venn-diagram
vennData <- list(A = element(A),
                 B = element(B),
                 C = element(C))

ggVennDiagram(vennData)


## Self-Checking 5
AuB <- union2(A,B)
AuC <- union2(A,C)
BuC <- union2(B,C)
AuBuC <- union2(AuB,C)

pprt(AuB, nrow(S))
pprt(AuC, nrow(S))
pprt(BuC, nrow(S))
pprt(AuBuC, nrow(S))

## Self-Checking 6
cprt(A, B)
cprt(A, C)
cprt(A, BC)
cprt(A, BuC)



## Self-Checking 7
S <- rolldie2(5)

evenEventFunction <- function(x){
  (sum(x)%%2)==0
} 

span5EventFunction <- function(x){
  (max(x)-min(x))==5
}

evenEventFunction(S[1,])
span5EventFunction(S[1,])

evenEventFunction(S[6,])
span5EventFunction(S[6,])

A <- subset(S, apply(S, 1, evenEventFunction))
nrow(A)

B <- subset(S, apply(S, 1, span5EventFunction))
nrow(B)

N <- nrow(S)

cprt2 <- function(a, an, b, bn) {
  ab = intersect2(a, b)
  cat(paste0("P(",an,"|",bn,") ="), nrow(ab),"/",nrow(b),
      "=", nrow(ab)/nrow(b))
}

pprt2 <- function(x, xn, n, prt=TRUE) {
  if (prt==TRUE) cat(paste0("P(", xn, ") ="), nrow(x), "/", n, 
                     "=", nrow(x)/n,"\n")
  invisible(nrow(x)/n)
}

indep.event(A, B, N)


## Self-Checking 8
prior <- c(0.2, 0.4, 0.3, 0.1)
cond <- c(4, 2, 1, 5)/100
tot <- prior*cond
tot
stot <- sum(tot)



## Self-Checking 9
post <- tot / stot
bayes.plot(prior, post)



######################################################
## Lab code for Chap.2 (Cont.)                     ##
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
