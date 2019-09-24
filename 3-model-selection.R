# model selection ####

# suppose we have data from an experiment like this:
# mean RT correct = 250ms
# mean RT incorrect = 246ms
# accuracy = 0.80

# try to fit this data with both models by adjusting the parameters of the model
# HINT: you can speed up your parameter search by using a small number of samples
# initially, and then increasing the samples as you get closer to a viable set
# of parameters.
# 2nd HINT: Don't adjust the sdrw parameter of the random.walk.model or the criterion
# paramter of the accumulator model.

# You don't need to get a perfect match. Just get in the ballpark. 


# Can both models do a reasonable job of accounting for the mean RT and accuracy? Report the
# results of your efforts:


# Using the parameters that you found above, plot histograms of the distribution of RTs
# predicted by each model. Based on these distributions, what kind of information could
# we use to evaluate which model is a better descriptor of the data for the experiment?
# Describe briefly how you might make this evaluation.


random.walk.model <- function(samples, drift=0, sdrw=0.3, criterion=3){
  
  
  data <- replicate(samples, sim.1(drift, sdrw, criterion))
  
  accuracy.array <- data[1, 1:samples]
  rt.array <- data[2, 1:samples]
  
  output <- data.frame(
    correct = accuracy.array,
    rt = rt.array
  )
  
  return(output)
}

sim.1 <- function(drift, sdrw, criterion){
  evidence <- 0
  time <- 0
  while (evidence < criterion && evidence > -criterion){
    evidence <- evidence + rnorm(1, drift, sdrw)
    time <- time + 1
  }
  return(c(evidence >= criterion, time))
}


r.walk.test <- random.walk.model(10000)
acc.test <- accumulator.model(10000)



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
    evidence.1 <- evidence.1 + repx(1, rate.1)
    evidence.2 <- evidence.2 + repx(1, rate.2)
    time <- time + 1
  }
  if(evidence.1 > evidence.2){
    return(TRUE, time)
  } else{
    return(FALSE, time)
  }
}



r.walk.test <- random.walk.model(10000)
acc.test <- accumulator.model(10000)

accuracy <- function(x){
  filter(x, TRUE)/length
}

accuracy(r.walk.test[1])


