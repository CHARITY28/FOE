######### Lab 10 ##################################################
##### Correlation Coefficient #####################################
## The two variables should be normally distributed
## This is referred to as bivariate normal distribution
## Before you calculate a correlation coefficient, use one of the
## methods you have learnt to check for the data normality.
## Shapiro-Wilks test if n =< 5000
## Histogram
## Scatter plot
## Scatter plot with histograms (density) or both on the margins
library(tidyverse)
install.packages("ggExtra")
library(ggExtra)
#### Example #####################################################

set.seed(123)
var1 <- rnorm(150, mean=2, sd=1)
var2 <- rnorm(150, mean=3, sd=1) - 0.25*var1
var3 <- rnorm(150, mean=3, sd=2) + 0.86*var1
data <- data.frame(var1, var2, var3)

# Save the scatter plot in a variable
p <- ggplot(data, aes(x = var1, y = var2)) +
  geom_point() +
  geom_smooth(method = "loess",
              se = FALSE)

# Plot the scatter plot with marginal histograms
ggMarginal(p, type = "histogram")
ggMarginal(p, type = "histogram", binwidth = 0.5)
ggMarginal(p, type = "density",
           lwd = 1.5)
ggMarginal(p, type = "densigram",
           lwd = 1.5,
           fill = "red")

#
qqPlot(data$var1)
qqPlot(data$var2)
qqPlot(data$var3)

cor(data$var1, data$var2)
cor(data$var1, data$var3)

set.seed(123)
var1 <- rnorm(150, mean=2, sd=1)
var2 <- rnorm(150, mean=3, sd=1) - 0.4*var1
var3 <- rnorm(150, mean=3, sd=2) + 1.5*var1
data <- data.frame(var1, var2, var3)

cor(data$var1, data$var2)
cor(data$var1, data$var3)

set.seed(123)
var1 <- rnorm(150, mean=2, sd=1)
var2 <- rnorm(150, mean=3, sd=1) - 1.1*var1
var3 <- rnorm(150, mean=3, sd=2) + 3.1*var1
data <- data.frame(var1, var2, var3)

### What can you deduce from above that two variables are not bivariately distributed
### So to measure the correlation we use spearman correlation 

cor(data$var1, data$var2)
cor(data$var1, data$var3)

############# Hypothesis testing ###########################
############# Proportions ##################################
n1 <- 100 # sample size of group 1
n2 <- 150 # sample size of group 2
s1 <- 32 # Number of successes in group 1
s2 <- 41 # Number of successes in group 2
p <- 0.38 # the value we want to test around it

#### One sample proportion #################################
#### Normality of data test ################################
# Null Hypothesis (H0):        p = 0.38
# Alternative Hypothesis (Ha): p != 0.38

# Success-Failure Conditions #
p_hat1 = s1/n1 # probability of success

n1*p_hat1 >= 10
n1*(1 - p_hat1) >= 10

# both conditions should be true for normal approximation and
# the CLT to hold

# we take the first group n1 and s1
prop.test(x = 32, n = 100, p = p)

### Hypothesis testing based on normal approximation and CI ###
## 95% CI
z_star <- qnorm(0.95) # confidence significance two-tailed
se <- sqrt((p_hat1*(1-p_hat1))/n1) # standard error for a proportion
me <- z_star*se # margin of error
ub <- p_hat1 + me
lb <- p_hat1 - me
ci <- c(lb, ub)
ci

## Compare the confidence intervals of the prop.test (Chi-squared based)
## and the normal approximation based CI

############# Numerical Data ###############################

set.seed(123)
var1 <- rnorm(150, mean=2, sd=1) + rnorm(150, mean = 0.1, sd = 0.1)
var2 <- rnorm(150, mean=3, sd=1.3) + runif(150, -1.5, 1)
var3 <- rnorm(150, mean=3, sd=2.5)
data <- data.frame(var1, var2, var3)

qqPlot(data$var1)
qqPlot(data$var2)
qqPlot(data$var3)


## New data with var2 and var3 only for two-samples mean
var2a <- rep("var2", 150)
var3a <- rep("var3", 150)

var <- append(var2a, var3a)  
value <- append(var2, var3)
data2 <- data.frame(var,value)
data2
############## One-sample Mean ##############################
#### Shapiro-Wilk test
#### H0: variable is normally distributed in some population.
#### Ha: variable NOT normally distributed 
### P<0.05 we reject the null hypothesis 

## Check for data normality
shapiro.test(data$var1)

## t-test for one-sample
## We want to check if the sample mean of var 1 = 2.25
## H0: mu =  2.2
## Ha: mu != 2.2
mean(data$var1)

t.test(data$var1, mu = 2.25, conf.level = 0.95)

## Calculating confidence interval for one-sample mean
## 99% condifence interval

mu_hat = mean(var1)
sd_hat = sd(var1)
n = length(data$var1)
se = sd_hat/sqrt(n)
t_star = qt(0.99, df = n-1) # two-tails
me = t_star*se

ub <- mu_hat + me
lb <- mu_hat - me

ci <- c(lb, ub)
ci

# compare manually calculated confidence interval 
# with the one calculated by the t.test() function

############# Two-independent samples #####################
## we want to compare the difference between two independent
## samples means. Are they equal or different.
## We want to compare the means of var2 with var3
## H0: mu(var2) = mu(var3)
## H1: mu(var2) != mu(var3)

data2 %>%
  group_by(var) %>%
  summarise(mean = mean(value), sd = sd(value))

mu_hat1 = mean(var2)
mu_hat2 = mean(var3)
sd_hat1 = sd(var2)
sd_hat2 = sd(var3)
n1 = length(data$var2)
n2 = length(data$var3)

##### Check the normality of each variable #####
# Shapiro-Wilk normality test for Professionals' Timing
with(data2, shapiro.test(value[var == "var2"]))
# Shapiro-Wilk normality test for Amateurs' Timing
with(data2, shapiro.test(value[var == "var3"]))

#### Equality of variance ######################
## When we compare between two groups, it is not enough
## to check for normality. We need to check if the ratio
## between the variances between the two groups is 1.
## This is called the equlaity of variance condition.
## If the variances are equal, we proceed with the default method
## other wise we need to specify the differences between the 
## two variances.

## Let's check the hypothesis of the two variance are equal at
## 99% level of confidence (1% level of statistical significance)

# H0: true ratio of variances is equal to 1
# Ha: true ratio of variances is not equal to 1

var.test(value ~ var, data = data2)

## The p-value is very small 2.3*10^-9 < 1%
## then we reject the null hypothesis the variances are equal.

### Comparison between the two means
## In the t.test() function, it is important to specify the
## argument of variance equality var.equal  to be FALSE

t.test(value ~ var, data = data2, var.equal = FALSE)

############ Two-dependent samples ########################
## Suppose we have trainees at your company. The trainees were
## assessed based on a perfromance scale. One of these scales
## was how many words they can type in a minute. You provided
## your trainees with a course of professional typing. Then
## you assessed their ability again. This type of data is called
## paired-data. It means the data is paired in the same subject.
## It has a pre and post measures. Since the data is dependent
## it cannot be treated as two different groups!

## Example:
# can the professional typing course improve the ability
# of the participants by an average of 7 words?!

data <- read_csv("paired.csv")

### Check for normality.
# The normality is checked for the difference!
# Take the difference between the pre and post for each observation
# In our data, I used pivot wider to create two columns
# then I created a new column for the difference.

data2 <- data %>%
  pivot_wider(names_from = Stage, values_from = NoW) %>%
  mutate(diff = Post - Pre)

shapiro.test(data2$diff)

mean(data2$diff)

# H0: Mean(difference) =  7
# Ha: Mean(difference) != 7

### The question is

t.test(NoW ~ Stage, data = data, mu = 7, paired = TRUE)

### Using confidence interval
mu_hat = mean(data2$diff)
sd_hat = sd(data2$diff)
n = length(data2$diff)

se = sd_hat/sqrt(n)
t_star = qt(0.975, df = 34)
me = t_star*se

ub = mu_hat + me
lb = mu_hat - me

ci <- c(lb, ub)
ci

############# Chi-sqaured goodness of fit test ############
mysample <- c(10, 15, 25, 50)
prob <- c(0.12, 0.18, 0.22, 0.48)
chisq.test(mysample, p = prob)

mysample <- c(10, 15, 25, 50)
prob <- c(0.05, 0.25, 0.22, 0.48)
chisq.test(mysample, p = prob)

############# Chi-squred Test of Independence #############
### Ho: the two variables are independent-- there is no relationship between them
### Ha: The two variables are NOT independent-- There is a relationship between them
### Chi-squared test p<0.05 we reject the null hypothesis

sex <- c("Male", "Female", "Male", "Female", "Male",
         "Female", "Male", "Female", "Male", "Female",
         "Female", "Female", "Male", "Female", "Male", 
         "Male", "Female", "Female", "Male", "Female", 
         "Male", "Female", "Female", "Male", "Female", 
         "Male", "Female", "Male", "Female", "Male",
         "Female", "Female", "Female", "Male", "Female",
         "Male", "Male", "Female", "Male", "Female",
         "Male", "Female", "Male", "Female", "Female")

flavour <- c("Choco", "Vanilla", "Vanilla", "Mix", "Mix",
             "Vanilla", "Mix", "Mix", "Choco", "Mix",
             "Choco", "Mix", "Vanilla", "Choco", "Mix",
             "Choco", "Vanilla", "Mix", "Choco", "Mix",
             "Vanilla", "Vanilla", "Choco", "Mix", "Mix",
             "Choco", "Vanilla", "Choco", "Mix", "Vanilla",
             "Vanilla", "Choco", "Choco", "Choco", "Mix",
             "Mix", "Vanilla", "Choco", "Choco", "Mix",
             "Mix", "Vanilla", "Mix", "Mix", "Vanilla")

data <- data.frame(sex, flavour)
addmargins(table(data))

chisq.test(table(data))


sex <- c("Male", "Female", "Male", "Female", "Male",
         "Female", "Male", "Female", "Male", "Female",
         "Female", "Female", "Male", "Female", "Male", 
         "Male", "Female", "Female", "Male", "Female", 
         "Male", "Female", "Female", "Male", "Female", 
         "Male", "Female", "Male", "Female", "Male",
         "Female", "Female", "Female", "Male", "Female")

flavour <- c("Choco", "Vanilla", "Vanilla", "Mix", "Mix",
             "Vanilla", "Mix", "Mix", "Choco", "Mix",
             "Choco", "Mix", "Vanilla", "Choco", "Mix",
             "Choco", "Vanilla", "Mix", "Choco", "Mix",
             "Vanilla", "Vanilla", "Choco", "Mix", "Mix",
             "Choco", "Vanilla", "Choco", "Mix", "Vanilla",
             "Vanilla", "Choco", "Choco", "Choco", "Mix")

data <- data.frame(sex, flavour)
table(data)

chisq.test(table(data))

fisher.test(table(data))
