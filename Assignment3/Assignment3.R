install.packages('dslabs')

library(dslabs)


data(murders)

# Research question: Is the gun murder rate (proportion of gun murders to state population)
# the same in all 4 regions of USA?

# Statistical tests: One-way ANOVA and Kruskall-Wallis test


# H0: µ1 = µ2 = µ3  = ...   = µk   ("all k population means are equal")
# H1: At least one µi different  ("at least one of the k population means is not equal to the others")

#where

#µi is the population mean of the ith group (i = 1, 2, ..., k)


library(dplyr)

murders$rate <- murders$total / murders$population

murders %>% group_by(region) %>%
  summarise(
    mean = mean(rate)
    )

# One-way ANOVA

# http://www.sthda.com/english/wiki/one-way-anova-test-in-r 

# Compute the analysis of variance
res.aov <- aov(rate ~ region, data = murders)
# Summary of the analysis
summary(res.aov)

# There is strong evidence against H_0, so not all k population means are equal




# Kruskall-Wallis test

# http://www.sthda.com/english/wiki/kruskal-wallis-test-in-r 

kruskal.test(rate ~ region, data = murders)



# Use the dataset from dslabs to inform the properties of your simulated data.

hist(murders$total)

hist(murders$population)

hist(murders$rate)

library(vcd)
library(MASS)

# estimate the parameters
fit1 <- fitdistr(murders$rate, "gamma") 

# goodness of fit test
ks.test(murders$rate, "pgamma", fit1$estimate) # p-value > 0.05 -> distribution not refused

# plot a graph
hist(ex, freq = FALSE, breaks = 100, xlim = c(0, quantile(ex, 0.99)))
curve(dexp(x, rate = fit1$estimate), from = 0, col = "red", add = TRUE)



mean <- mean()

# Develop scenarios for data generation (see workflow for examples).

# Write code to generate simulated data under different scenarios.

# Apply your chosen statistical tests to the simulated data.

# Calculate the size and power of these statistical tests under each of the scenarios you have developed.

# A significant Kruskal–Wallis test indicates that at least one sample stochastically dominates
# one other sample. The test does not identify where this stochastic dominance occurs or for how
# many pairs of groups stochastic dominance obtains. For analyzing the specific sample pairs for
# stochastic dominance, Dunn's test,[5] pairwise Mann–Whitney tests with Bonferroni correction,
# [6] or the more powerful but less well known Conover–Iman test[6] are sometimes used.






# The murders dataset from the dslabs package contains statistics on gun murder data from
# 2010 organized by each state in the United States of America.
# In the dataset, one can find the US state, the abbreviation of the US state,
# the geographical region of the state, the state population in 2010, as well as the total number
# of gun murders in the state in 2010. 

# https://rstudio-pubs-static.s3.amazonaws.com/555272_0c78ceeaafb1405f8e3484fc87615eac.html