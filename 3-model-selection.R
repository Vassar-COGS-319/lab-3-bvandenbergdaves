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


random.walk.model <- function(samples, drift=0.0115, sdrw=0.3, criterion=5.58){
  
  
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
  return(c((evidence >= criterion), time))
}




accumulator.model <- function(samples, rate.1=101, rate.2=109, criterion=3){
  
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



r.walk.test <- random.walk.model(10000)
acc.test <- accumulator.model(10000)

accuracy <- function(x){
  length <- length(x$correct)
  acc <- 0
  for (i in 1:length) {
    if(x$correct[i]){
      acc <- acc + 1
    }
  }
  acc/length
}

mean.correct.time <- function(x){
  length <- length(x$correct)
  acc <- 0
  for (i in 1:length) {
    if(x$correct[i]){
      acc <- acc + x$rt[i]
    }
  }
  acc/length
}

mean.incorrect.time <- function(x){
  length <- length(x$correct)
  acc <- 0
  for (i in 1:length) {
    if(!(x$correct[i])){
      acc <- acc + x$rt[i]
    }
  }
  acc/length
}



r.walk.correct.time <- array()
end.length <- 0
for (i in 1:length(r.walk.test$rt)) {
  if(r.walk.test$correct[i]){
    end.length <- end.length + 1
    r.walk.correct.time[end.length] <- r.walk.test$rt[i]
  }
}

r.walk.incorrect.time <- array()
end.length <- 0
for (i in 1:length(r.walk.test$rt)) {
  if(!(r.walk.test$correct[i])){
    end.length <- end.length + 1
    r.walk.incorrect.time[end.length] <- r.walk.test$rt[i]
  }
}



acc.correct.time <- array()
end.length <- 0
for (i in 1:length(acc.test$rt)) {
  if(acc.test$correct[i]){
    end.length <- end.length + 1
    acc.correct.time[end.length] <- acc.test$rt[i]
  }
}

acc.incorrect.time <- array()
end.length <- 0
for (i in 1:length(acc.test$rt)) {
  if(!(acc.test$correct[i])){
    end.length <- end.length + 1
    acc.incorrect.time[end.length] <- acc.test$rt[i]
  }
}

accuracy(r.walk.test)
mean.correct.time(r.walk.test)
mean.incorrect.time(r.walk.test)

accuracy(acc.test)
mean.correct.time(acc.test)
mean.incorrect.time(acc.test)


hist(r.walk.correct.time)
hist(r.walk.incorrect.time)
hist(acc.correct.time)
hist(acc.incorrect.time)

# These results are both pretty far from the experimental data. It seems that both models
# predict that when accuracy is high the mean correct time will be much higher than the
# mean incorrect time. This is probably because both models rely on a form of drift rate
# to determine the accuracy of the process.When the drift rate strongly favors one
# result, the likelihood that that will be the chosen result increases as a function of
# time. That means the less favored result is only likely to occurr in the early stages
# when there is minimal evidence. As time passes and evidence increases, the law of
# averages takes over, and the likelihood of getting the less favored outcome drops.

# The data from the histograms show that the accumulator model give runtimes that fall
# into a normal distribution while the random walk model gives runtimes that are heavily
# skewed towards short runtimes. If the experimental runtime data falls in a normal
# distribution then the accumulator model describes it better. If it is instead skewed
# towards shorter runtimes, then the random walk model would be better.