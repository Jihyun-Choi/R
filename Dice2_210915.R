##################################
## Lecture Material             ##
## for Probability & Statistics ##
## Minseok Seo                  ##
## 2021-09-15                   ##
##################################

setRepositories(ind = 1:7) 
#setRepositories() 를 써야 R과 관련된 여러가지 패키지를 사용할 수 있다.
#이 라이브러리를 써야 우리가 R로 하는 모든 것들을 할 수 있다.  가장 기본이 되는 라이브러리

#필요한 패키지와 라이브러리 실행
# 한번 install을 하면 추후 생성하는 다른 스크립트에서도 관련해서 사용할 수 있으나 
# 추후 해당 스크립트만 다운받아 실행 시 install과 libarary를 작성하지 않았다면 실행이 되지않는 오류가 생길 수 있음.
# 고로 install은 굳이 작성할 필요가 없으나 스크립트마다 사용한 library를 적는 것이 좋다!

install.packages("devtools")
library(devtools)

install_github('jhk0530/Rstat')
install.packages("ggVennDiagram")
install.packages("animation")

library(Rstat)
library(ggVennDiagram)
library(animation)
library(dplyr)


## Set two fair die (Self-Checking 1) ##----------1
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


## For combination
S

for(i in 1:nrow(S)){
  S[i,] <- sort(as.numeric(S[i,]))
}

distinct(S)
S <- distinct(S)
nrow(S)

## Get Event A   - subset()
eventA <- subset(S, ((Dice_1+Dice_2) %% 2)==0)
element(eventA)

## Get Event B
eventB <- subset(S, (Dice_1+Dice_2) >=8)
element(eventB)

## Get Event C
eventC <- subset(S, abs(Dice_1 - Dice_2) <= 1)  #abs() <-절댓값
element(eventC)


## intesect
AB <- intersect2(eventA, eventB)
AC <- intersect2(eventA, eventC)
BC <- intersect2(eventB, eventC)

## Complement   - setdiff()를 사용해 여집합 구하기
Ac <- setdiff(S, eventA)
Bc <- setdiff(S, eventB)
Cc <- setdiff(S, eventC)

## Drawing Venn-diagram
vennData <- list(A = element(eventA),
                 B = element(eventB),
                 C = element(eventC))
ggVennDiagram(vennData)


## Simulation of law of large numbers (Self-Checking 2) ##----------2
ani.options(nmax = 1000, interval = 0.00001)
win.graph(7,4)

lln.ani(FUN = function(n, mu) rbinom(n, size=1, prob = mu), mu = 0.5, type="n", col.poly="blue")

title(main = "Law of Large Numbers (Korea Univ.)")



## Self-Checking non-amination version
nTimes <- 100000  #실행 횟수

eventCoin <- c()  #eventCoin이라는 변수를 사용하기 위한 빈 공간을 만든다는 개념

#rbinom(1, size=1, prob = 0.5) <- 0이나 1이 prob(확률)0.5로 생성 

for(i in 1:nTimes){
  eventCoin[i] <- rbinom(1, size=1, prob = 0.5)
  print(paste0(i, " times drawing... Prob(Head) = ", (sum(eventCoin == 1) / length(eventCoin))))
}

# eventCoin <- c() 와 관련한 추가 설명
a <- "a"
b <- "b"
#ab <- "a", "b" #이 코드는 오류, 두개를 한번에 넣을 수 없음
ab <- c("a", "b") #이렇게 c()로 묶어주면 사용가능



