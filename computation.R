# R. Teal Witter
# Math 710 Spring 2019
# Cover Time of Birthdays
library(tidyverse)
library(ggplot2)

trials <- 1000         # Number of trials
n <- 365               # Number of 'vertices' to cover
cover_times <- c()
for (i in 1:trials) {
  x <- 0
  birthdays <- 1:n # Unvisited birthdays
  # Count number of people until every birthday is covered
  while (length(birthdays) > 0) {
    birthday <- sample(1:n, 1, replace=TRUE)      # Uniform birthdays
    birthdays <- birthdays[birthdays != birthday] # Remove birthday
    x <- x+1                                      # Count steps
  }
  cover_times[i] <- x
}

cover_times2 <- data.frame(cover_times)

cover_times3 <- cover_times2%>% # Append percentiles
  mutate(percentile=ecdf(cover_times)(cover_times))

cdf <- function(r) { # Analytical result of cover time CDF
  x <- (r - n * log(n)) / n
  return (exp(-exp(-x)))
}

cdf(2190) # Probability all birthdays represented with 2190 people
cdf(1825) # Probability all birthdays represented with 1825 people

cover_times3 %>% # Graph of birthday cover times
  ggplot() + theme_classic()+
  geom_line(aes(x=cover_times3$cover_times, y=cover_times3$percentile),
            color="red", alpha=.5, size=3) +
  stat_function(fun = cdf, color="blue", alpha=1, size=1) +
  ylab("") + xlab("")