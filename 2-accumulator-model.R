# implement the model by filling in the function below
# the model should return a data frame with two columns: correct and rt
# the correct column should be TRUE or FALSE, and rt should contain the
# number of steps it took to reach the criterion.

# note that the function takes four arguments:
# samples is the number of samples to draw from the model
# rate.1 is the evidence accumulation rate for the correct response (default value is 40)
# rate.1 is the evidence accumulation rate for the incorrect response (default value is 40)
# criterion is the threshold for a response (default value is 3)

# one oddity: note that higher values for rate.1 and rate.2 will actually produce slower RTs.
# this is because the rate parameter is controlling the rate of decay of the exponential distribution,
# so faster rates mean that less evidence is likely to accumulate on each step. we could make
# these parameters more intuitive by taking 1/rate.1 and 1/rate.2 as the values to rexp().

accumulator.model <- function(samples, rate.1=40, rate.2=40, criterion=3){
  
  data <- replicate(samples, sim.2(rate.1, rate.2, criterion))
  
  accuracy.array <- data[1, 1:samples]
  rt.array <- data[2, 1:samples]

  output <- data.frame(
    correct = accuracy.array,
    rt = rt.array
  )
  
  return(output)
}


sim.2 <- function(rate.1, rate.2, criterion){
  evidence.1 <- 0
  evidence.2 <- 0
  time <- 0
  while (evidence.1 < criterion && evidence.2 < criterion){
    evidence.1 <- evidence.1 + rexp(1, rate.1)
    evidence.2 <- evidence.2 + rexp(1, rate.2)
    time <- time + 1
  }
  if(evidence.1 > evidence.2){
    return(c(TRUE, time))
  } else{
    return(c(FALSE, time))
  }
}


# test the model ####

# if the model is working correctly, then the line below should generate a data frame with 
# 1000 samples and about half of the samples should be correct. the average rt will probably
# be around 112, but might vary from that by a bit.

initial.test <- accumulator.model(1000)
sum(initial.test$correct) / length(initial.test$correct) # should be close to 0.5
mean(initial.test$rt) # should be about 112

# visualize the RT distributions ####

# we can use dplyr to filter the data and visualize the correct and incorrect RT distributions

library(dplyr)

correct.data <- initial.test %>% filter(correct==TRUE)
incorrect.data <- initial.test %>% filter(correct==FALSE)

hist(correct.data$rt)
hist(incorrect.data$rt)


# CORRECTIONS

# My solution works. No revisions needed.
