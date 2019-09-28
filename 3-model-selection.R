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

# I would say that both models do a reasonable job of accounting for mean RT & accuracy.

# Random-Walk Model
# Used: drift=0.01, sdrw=0.3, criterion=4.8
    # The model output: 
    # mean RT correct = 249 ms
    # mean RT incorrect = 238 ms
    # accuracy = 0.748

# Accumulator Model
# Used: rate.1 = 77, rate.2 = 84 
    # The model output:
    # mean RT correct = 255 ms
    # mean RT incorrect = 244 ms
    # accuracy = 0.81

# Using the parameters that you found above, plot histograms of the distribution of RTs
# predicted by each model. Based on these distributions, what kind of information could
# we use to evaluate which model is a better descriptor of the data for the experiment?
# Describe briefly how you might make this evaluation.

# We could use information about the distribution of reaction times to evaluate which
# model is a better descriptor of data. While the histogram for the random-walk model
# demonstrates an exponential curve, the accumulator model shows a normal distribution
# over the mean RT. I would make this evaluation based on its implications on the 
# nature of decision-making.  

acc.test <- accumulator.model(1000)
acc.correct.data <- acc.test %>% filter(correct==TRUE)
acc.incorrect.data <- acc.test %>% filter(correct==FALSE)

hist(acc.correct.data$rt)
hist(acc.incorrect.data$rt)

rwalk.test <- random.walk.model(1000)
rwalk.correct.data <- rwalk.test %>% filter(correct==TRUE)
rwalk.incorrect.data <- rwalk.test %>% filter(correct==FALSE)

hist(rwalk.correct.data$rt)
hist(rwalk.incorrect.data$rt)
