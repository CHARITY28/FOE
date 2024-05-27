############ permutation #############################################
### Example chosing 4 items without replacement with order out of 12
prod(12:(12-3))
############ Combinations ############################################
### ICs example
paste0(round(choose(75,12)/choose(100,12),4)*100,"%")
############# Birthday Paradox #######################################
## The birthday problem
k <- 2 #class size = 2
prod(365:(365-k+1))/365^k #all different
1 - prod(365:(365-k+1))/365^k #at least 2 the same

k <- 10 #class size = 10
prod(365:(365-k+1))/365^k #all different
1 - prod(365:(365-k+1))/365^k #at least 10 the same

k <- 20 #class size = 20
prod(365:(365-k+1))/365^k #all different
1 - prod(365:(365-k+1))/365^k #at least 20 the same

k <- 30 #class size = 30
prod(365:(365-k+1))/365^k #all different
1 - prod(365:(365-k+1))/365^k #at least 30 the same

k <- 40 #class size = 40
prod(365:(365-k+1))/365^k #all different
1 - prod(365:(365-k+1))/365^k #at least 40 the same

############ Simulation ###########################################
## Simulating tossing a coin 20 times
s <- c("H","T")
n <- 20
set.seed(8)
Sn <- sample(s, size = n, replace = TRUE)
table(Sn)
prop.table(table(Sn))

## Increase the repetitions to 100
n <- 100
Sn <- sample(s, size = n, replace = TRUE)
table(Sn)
prop.table(table(Sn))

## increase the repetition to 1000
n <- 1000
Sn <- sample(s, size = n, replace = TRUE)
table(Sn)
prop.table(table(Sn))

## increase the repetition to 10000
n <- 100000
Sn <- sample(s, size = n, replace = TRUE)
table(Sn)
prop.table(table(Sn))

# with increased repetition, we approach the expected value 0.5

####### Visualisaing relative frequency approach for probability #########
#### Law of Large Numbers ################################################
tossCoin <- function(n, p=1/2) {
  
  # create a probability distribution, a vector of outcomes
  # the side we want (head or tail) is 1 and 0 otherwise
  # and their associated probabilities
  outcomes = c(0,1) # sample space
  probabilities = c(1-p,p)
  
  # create a random sample of n tosses
  tosses = sample(outcomes,n,replace=TRUE,prob=probabilities)
  
  # now create a cumulative mean vector
  cum_sum = cumsum(tosses)
  index = c(1:n)
  prob = cum_sum / index
  
  # now combine the index, tosses and prob vectors
  # into a data frame and return it
  return(data.frame(index,prob))
}

data <- tossCoin(1000,1/2)

plot(data$index, data$prob, type = "l",
     xlab = "Toss number", ylab = "prob")
abline(0.5,0, col = "red", lwd = 2)