########################################################################
#                      Poisson Distribution

# Content: Density, distribution function, and random generation 
#          for the Poisson distribution with parameter lambda(=mu).

# Density function: dpois(x, lambda, log = FALSE)
# Distribution function (cumulative probability): ppois(q, lambda, lower.tail = TRUE, log.p = FALSE)
# Random Generation: rpois(n, lambda)  

########################################################################


rm(list=ls())     # Clear the Environment


# Example:

# RDA investigated that there are four cars crossing a bridge per minute on average.

#(a) Find the probability of having 
#   (i) no cars
#   (ii) three or more cars
#   (iii) less than or equal 7 cars
#   crossing the bridge in a particular minute.

#(b) Plot the probability distribution of No of cars crossing the bridge.

# X = The number of cars crossing the bridge in a particular minute
# x = 0, 1, 2, 3,.....

# Poisson distribution with parameter lambda(=mu).
# X follows a Poisson(lamda = 12) distribution


help(dpois)

#(i) no cars: X=0
x = 0
# (i) P(X = 0)
P_X_0 <- dpois(x=0, lambda=4)

# dpois(0,4)
P_X_0

sprintf("P(X = 0) = %s", round(P_X_0, digits = 6))
sprintf("The probability of no cars crossing the bridge in a minute is %s", round(P_X_0, digits = 6))



#(ii) three or more cars: X >= 3 this is we can say more than two cars
# (ii) P(X >= 3) = 1 - P(X <= 2)
#P(X≥3)=1−P(X<3)=1−(P(X=0)+P(X=1)+P(X=2))
P_X_geq_3 <- 1 - ppois(2, lambda=4)   # lower tail 
P_X_geq_3

# P(X >= 3) or we can say P(x>2)
P_X_GEQ_3 <- ppois(2, lambda=4, lower=FALSE)   # upper tail,   here not (1-ppois no need to do, lower value is 2)
P_X_GEQ_3
sprintf("P(X >= 3) = %s", round(P_X_GEQ_3, digits = 4))


#(iii) less than or equal 7 cars
# (iii) P(X <= 7) 
P_X_leq_7 <- ppois(7, lambda=4)   # lower tail 
P_X_leq_7
sprintf("P(X <= 7) = %s", round(P_X_leq_7, digits = 4))

# what happen if p(x>3)     P(x>3)=1-p(x<=3)
x<-1-ppois(3,4)
x
#or
xy<- ppois(3,4,FALSE)
xy

#(iii) less than 7 cars  P(x<7)

P_X__7 <- ppois(6, lambda=4)   # lower tail 
P_X__7


# (b) The Poisson probability distribution plot

x <- 0:10

par(mfrow = c(2, 1))

# Probability density function (pdf)
barplot(dpois(x, lambda=4),col = "red",names.arg=x,
        xlab = "X = No of cars crossing the bridge", ylab = "pdf: P(X = x)",
        main="Poisson (mu = 4) pdf")

# Cumulative density function (cdf)
barplot(ppois(x, lambda=4),col = "blue",names.arg=x,
        xlab = "X = No of cars crossing the bridge", ylab = "cdf:P(X <= x)",
        main="Poisson (mu = 4) cdf")

############################### From chat gpt #########################

# Load necessary library
if(!require(ggplot2)) install.packages("ggplot2", dependencies=TRUE)
library(ggplot2)

# Define the parameter lambda for the Poisson distribution
lambda <- 4

# (a) Probability Calculations

# (i) Probability of no cars (k = 0)
prob_no_cars <- dpois(0, lambda)

# (ii) Probability of three or more cars (k >= 3)
prob_three_or_more_cars <- 1 - ppois(2, lambda)

# (iii) Probability of less than or equal to 7 cars (k <= 7)
prob_less_equal_seven_cars <- ppois(7, lambda)

# Print the probabilities
cat("Probability of no cars (k = 0):", prob_no_cars, "\n")
cat("Probability of three or more cars (k >= 3):", prob_three_or_more_cars, "\n")
cat("Probability of less than or equal to 7 cars (k <= 7):", prob_less_equal_seven_cars, "\n")

# (b) Plotting the probability distribution

# Values for the number of cars (0 to 15 for visualization purposes)
x <- 0:15
# Corresponding probabilities
y <- dpois(x, lambda)

# Create a data frame for plotting
data <- data.frame(Number_of_Cars = x, Probability = y)

# Plotting using ggplot2
ggplot(data, aes(x = Number_of_Cars, y = Probability)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.7) +
  labs(title = "Probability Distribution of Number of Cars Crossing the Bridge in a Minute",
       x = "Number of cars", y = "Probability") +
  theme_minimal() +
  geom_text(aes(label=round(Probability, 3)), vjust=-0.5, size=3)


#The plot shows the probability distribution of the number of cars crossing the bridge in a minute, with 
#𝜆=4  The probabilities peak around the mean (4 cars per minute) and decrease as the number of cars increases or decreases from the mean.

########################################################################
# Extra: Random generation for a Poisson distribution with parameter lambda(=mu).

# rpois(n, lambda)  


#Create a data set of 30 samples from a Poisson distribution with lambda = 6.23 
set.seed(2)     # to get the sample 
rpois(n=30, lambda = 6.23)



########################################################################
# Exercise:

# The number of accidents that occur at a busy intersection is Poisson distributed 
# with a mean of 3.5 per week. Find the probability of the following events:
# (a) Less than three accidents in a week
# (b) Five or more accidents in a week
# (c) No accidents in a week
# (d) Plot the probability distribution of number of accidents that occur in a week for 0 to 8 weeks.


#a P(x<3)=P(x<=2)=p(x=0)+p(x=1)+(x=2)
x_less_than_3<-ppois(2,3.5,TRUE)
x_less_than_3


#b P(x>=5)=P(x>4)
#P(X≥5)=1−P(X<5)=1−(P(X=0)+P(X=1)+P(X=2)+P(X=3)+P(X=4))
x_5_or_more1<-1-ppois(4,3.5,TRUE)
x_5_or_more1

#P(X≥5)=1−P(X<5)=1−(P(X=0)+P(X=1)+P(X=2)+P(X=3)+P(X=4))
x_5_or_more<-ppois(4,3.5,FALSE)
x_5_or_more

#P(x=0)
x_0<-dpois(0,3.5)
x_0

x <- 0:8

par(mfrow = c(2, 1))


# Probability density function (pdf)
barplot(dpois(x, lambda=3.5),col = "red",names.arg=x,
        xlab = "X = No of accident", ylab = "pdf: P(X = x)",
        main="Poisson (mu = .35) pdf")



# from chat gpt=================================================================
# Load necessary library
if(!require(ggplot2)) install.packages("ggplot2", dependencies=TRUE)
library(ggplot2)

# Define the parameter lambda for the Poisson distribution
lambda <- 3.5

# (a) Probability of less than three accidents in a week (k < 3)
prob_less_than_three <- ppois(2, lambda)

# (b) Probability of five or more accidents in a week (k >= 5)
prob_five_or_more <- 1 - ppois(4, lambda)
prob_five_or_more
prob_five_or_more1 <- ppois(4, lambda,FALSE)
prob_five_or_more1
# (c) Probability of no accidents in a week (k = 0)
prob_no_accidents <- dpois(0, lambda)

# Print the probabilities
cat("Probability of less than three accidents in a week (k < 3):", prob_less_than_three, "\n")
cat("Probability of five or more accidents in a week (k >= 5):", prob_five_or_more, "\n")
cat("Probability of no accidents in a week (k = 0):", prob_no_accidents, "\n")

# (d) Plotting the probability distribution

# Values for the number of accidents (0 to 8 for visualization purposes)
x <- 0:8
# Corresponding probabilities
y <- dpois(x, lambda)

# Create a data frame for plotting
data <- data.frame(Number_of_Accidents = x, Probability = y)

# Plotting using ggplot2
ggplot(data, aes(x = Number_of_Accidents, y = Probability)) +
  geom_bar(stat = "identity", fill = "red", alpha = 0.7) +
  labs(title = "Probability Distribution of Number of Accidents in a Week",
       x = "Number of accidents", y = "Probability") +
  theme_minimal() +
  geom_text(aes(label=round(Probability, 3)), vjust=-0.5, size=3)
