##################################
## Lecture Material (Week15)    ##
## for Probability & Statistics ##
## Minseok Seo                  ##
## 2021-12-06                   ##
##################################


## Self-Checking 3 (t-test)

# Generate Data, Gender and Height
Gender <- factor(c(rep("Male", 100), rep("Female", 100)))
Height <- c(rnorm(100, 173, 2), rnorm(100, 162.3, 2))

Data <- data.frame(Height, Gender)
dim(Data)

t.test(Height~Gender, Data, alternative = "two.sided")
t.test(Height~Gender, Data, alternative = "less")
t.test(Height~Gender, Data, alternative = "greater")


## paired t-test
# Weight of the mice before treatment
before <-c(200.1, 190.9, 192.7, 213, 241.4, 196.9, 172.2, 185.5, 205.2, 193.7)
# Weight of the mice after treatment
after <-c(392.9, 393.2, 345.1, 393, 434, 427.9, 422, 383.9, 392.3, 352.2)
# Create a data frame
my_data <- data.frame( 
  group = rep(c("before", "after"), each = 10),
  weight = c(before,  after)
)

# compute the difference
t.test(before, after, paired = TRUE)
t.test(weight ~ group, data = my_data, paired = TRUE)


## Wilcoxon rank sum test
wilcox.test(Height ~ Gender, data = my_data, exact = FALSE)


## Permutation t-test
Gender <- factor(c(rep("Male", 30), rep("Female", 30)))
Height <- c(rnorm(30, 173, 2), rnorm(30, 173, 2))

Data <- data.frame(Height, Gender)
dim(Data)

obs_t_stat <- as.numeric(t.test(Height ~ Gender, Data)$statistic)

null_t_stat <- c()
numOfRepeat <- 1000

for(i in 1:numOfRepeat){
  null_t_stat[i] <- as.numeric(t.test(Height ~ sample(Gender), Data)$statistic)
}

Pval <- (sum(abs(null_t_stat) >= abs(obs_t_stat)) + 1) / numOfRepeat
Pval


## ANOVA and kruskal test for Multiple group comparisons
data("ToothGrowth")
View(ToothGrowth)

summary(aov(len ~ dose, data = ToothGrowth))[[1]][1,5]
kruskal.test(len ~ dose, data = ToothGrowth)$p.value


## Association test for two continuous Random variables
data(mtcars)

cor.test(mtcars$mpg, mtcars$wt, data = mtcars)$p.value
cor.test(mtcars$mpg, mtcars$wt, data = mtcars, method = "spearman")$p.value
summary(lm(mpg~wt, data = mtcars))$coef[2,4]


## Association test for two cateogorical Random variables
data <- read.csv("https://goo.gl/j6lRXD")  #Reading CSV
table(data$treatment, data$improvement)

chisq.test(data$treatment, data$improvement)$p.value


Disease <- factor(c(rep("Covid-19", 50), rep("Normal", 50)))
Vaccine <- factor(c(rep("Shot", 50), rep("None", 50)))
Gender <- factor(rep(c(rep("Female", 25), rep("Male", 25)), 2))
  
table(Disease, Vaccine)
chisq.test(Disease, Vaccine)$p.value

table(Disease, Gender)
chisq.test(Disease, Gender)$p.value


