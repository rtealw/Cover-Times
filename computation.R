# R. Teal Witter
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
    # Uniform birthdays
    birthday <- sample(1:n, 1, replace=TRUE)      
    # Remove birthday
    birthdays <- birthdays[birthdays != birthday] 
    # Count steps
    x <- x+1                                      
  }
  cover_times[i] <- x
}

# Append percentiles
cover_times_analyzed <- data.frame(cover_times) %>% 
  mutate(percentile=ecdf(cover_times)(cover_times))

# Analytical result of cover time CDF
cdf <- function(r) {
  x <- (r - n * log(n)) / n
  return (exp(-exp(-x)))
}

# Start of x axis
start_x <- min(cover_times_analyzed$cover_times)

cdf(2190) # Probability all birthdays represented with 2190 people
cdf(1825) # Probability all birthdays represented with 1825 people

# Figure of birthday cover times
cover_times_analyzed %>% 
  ggplot() + theme_classic() +
  # Analytical cover times
  stat_function(fun = cdf, color="blue", alpha=.5, size=2) +
  # Empirical cover times
  geom_line(aes(x=cover_times_analyzed$cover_times,
                y=cover_times_analyzed$percentile),
            color="red", alpha=.5, size=2) +
  ylab("") + xlab("") +
  # Dashed lines to indicate points of interest
  geom_segment(aes(x=2190, y=cdf(2190), xend=2190, yend=0),
               linetype="dashed") +
  geom_segment(aes(x=1825, y=cdf(1825), xend=1825, yend=0),
               linetype="dashed") +
  geom_segment(aes(x=start_x, y=cdf(1825), xend=1825, yend=cdf(1825)),
               linetype="dashed") +
  geom_segment(aes(x=start_x, y=cdf(2190), xend=2190, yend=cdf(2190)),
               linetype="dashed") +
  scale_x_continuous(expand = c(0, 0),
                     breaks=c(1825, 2190, 3000, 4000)) +
  scale_y_continuous(expand = c(0, 0),
                     breaks=c(.085, .25, .405, .5, .75, 1))

                     