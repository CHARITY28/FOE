############ Lab 9 ###################################
library(pacman)
p_load(tidyverse, moments, car)
##### Import the dataset #####
data <- read_csv("lab9.csv")

## The dataset shows results of diabetic patients and
## data collected from each patient in 12 Jordanian 
## Primary healthcare units distributed over 4 regions
## EA: East and South Amman
## WA: West and North Amman
## SZ: East and South Zarqa
## WZ: West and South Zarqa

## HH_income:
## 1  < 300 USD
## 2 300 - <600 USD
## 3 600 - <900 USD
## 4 900 - <1200 USD
## 5 >= 1200 USD

## Education: level of education
## 1 Primary School
## 2 Secondary School
## 3 College or higher

# Age: Rounded to full year
# Gender: Female, Male
# Rent: House is rented
# Loans: if the patient has loan(s)
# Smoking: if the patient smokes cigerattes, Hookeh (Nergileh), etc.
# Employment: if the patient is employed
# Marital_Status: Single, Married, Divorced, Widowed
# HbA1c: Heamoglobin Accumelated glucose level (%)

########### Data Exploration using R - The basics #####
#### Data tabualtion #####
#### One-way table #####
tbl1 <- table(data$Region)
tbl1

round(prop.table(tbl1),2)

plot(tbl1)


############ Measures of Central Tendency ###############
summary(data$Age)
summary(data$HbA1c)
quantile(data$Age, probs = seq(0,1,0.25))
quantile(data$Age, probs = seq(0,1,0.1))

########### Measures of Dispersion ######################
max(data$Age) - min(data$Age) # Range
IQR(data$HbA1c)
var(data$HbA1c)
sd(data$Age)
sqrt(var(data$HbA1c)) == sd(data$HbA1c)

mad(data$Age, center = mean(data$Age)) #Mean Absolute Deviation
mad(data$Age) # Default center = median(x) Median Absolute Deviation

########### Data Shape ####################################
### Checking for Normality ################################
## Q-Q plot - First Method
qqnorm(data$Age, pch = 1, frame = FALSE)
qqline(data$Age, col = "steelblue", lwd = 2)

qqnorm(data$HbA1c, pch = 1, frame = FALSE)
qqline(data$HbA1c, col = "darkred", lwd = 2)

## Q-Q plot - Second Method
qqPlot(data$Age)
qqPlot(data$HbA1c)

#### Transformation #######
qqPlot(log(data$Age))
qqPlot(sqrt(data$Age))
qqPlot((1/data$Age))
qqPlot((data$Age)^1/3)

### Before Transforming your data check the normality with
### Other methods

#### Histogram #################
hist(data$Age)
ggplot(data, aes(x = Age)) +
  geom_histogram() +
  theme_bw()

#### Checking potential outliers with boxplot ############
data %>%
  ggplot(aes(x = Age)) +
  geom_boxplot( outlier.alpha= 0.5,
               outlier.color = "red",
               outlier.size = 1.5) +
  theme_bw()

#### density #####################
data %>%
  ggplot(aes(x = Age)) +
  geom_density(fill = "steelblue", alpha = 0.6) +
  theme_bw()

##### Skewness ##################
skewness(data$Age)
kurtosis(data$Age) - 3 # Deviation from normal (Excess Kurtosis)

#### Jaruque-Bera Normality Test
jarque.test(data$Age)
jarque.test(data$HbA1c)

#### Shapiro test ####
shapiro.test(data$Age)
shapiro.test(data$HbA1c)


########## Bi-variate ########################################
### relationships ############################################

## Useful for studying relationships in many situations:
## 1- Correlation (numeric data)
## 2- Association (categorical data) - Chi-squared test
##                                    and Fisher's Exact test
##              (Will be covered in the course this week)
##
## 3- Hypothesis testing - We covered different types
## 4- Dimensionality reduction (will be covered in later courses)
## 5- Understanding your data.
## and many more issues.


#### Two-way table #####
tbl2 <- table(data$Region, data$Gender)
tbl2

round(prop.table(tbl2),2)

round(prop.table(tbl2, margin = 1),2)

round(prop.table(tbl2, margin = 2),2)

plot(tbl2)

plot(round(prop.table(tbl2, margin = 1),2))

plot(round(prop.table(tbl2, margin = 2),2))

##### Three-way Table ##########
## It is rarely used, but can be extremely useful
## One of the useful applications is startified analysis
## When you want to control for a potential confounding variable
## this is part of controlling for potential biases such as,
## something that we call Simpson's Paradox
## (Will be discussed in the data visualisation course)
## Some statistical tests can be applied for such situations
## It is not required in this course!

tbl3 <- table(data$Region, data$Gender, data$Education)

round(prop.table(tbl3),2)

plot(tbl3)

##### Scatter-plot ###################
data %>%
  ggplot(aes(x = Age, y = HbA1c)) +
  geom_point() +
  theme_bw()

data %>%
  ggplot(aes(x = Age, y = HbA1c)) +
  geom_point() +
  geom_smooth(method = "loess",
              se = FALSE) +
  theme_bw()

data %>%
  ggplot(aes(x = Age, y = HbA1c)) +
  geom_point() +
  geom_smooth(method = "lm",
              se = FALSE) +
  theme_bw()

#### Stratification of the data ##############

data %>%
  filter(data$Age < 70) %>%
  ggplot(aes(x = Age, y = HbA1c)) +
  geom_point() +
  geom_smooth(method = "loess",
              se = FALSE) +
  theme_bw()

data %>%
  filter(data$Age > 70) %>%
  ggplot(aes(x = Age, y = HbA1c)) +
  geom_point() +
  geom_smooth(method = "loess",
              se = FALSE) +
  theme_bw()

data %>%
  filter(data$Age < 70) %>%
  ggplot(aes(x = Age, y = HbA1c)) +
  geom_point() +
  geom_smooth(method = "lm",
              se = FALSE) +
  theme_bw()

data %>%
  filter(data$Age > 70) %>%
  ggplot(aes(x = Age, y = HbA1c)) +
  geom_point() +
  geom_smooth(method = "lm",
              se = FALSE) +
  theme_bw()

### There are two different relationships in our data
### This is an example of Simpson's paradox
### The overall relation seems to be no correlation
### stratifying the data shows different directions.

#### Create new variable for startification ###################
data2 <- data %>%
  mutate(stratum = ifelse(data$Age > 70, 1,0))

data2 %>%
  ggplot(aes(x = Age, y = HbA1c, color = factor(stratum))) +
  geom_point() +
  geom_smooth(method = "lm",
              se = FALSE) +
  theme_bw()
