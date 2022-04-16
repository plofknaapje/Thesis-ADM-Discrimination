# This script generates the population with variables G L1, L2, L3, X1 and X2.
library(tidyverse)
set.seed(666)

pop_size <- 1000
score_values <- c(0, 1, 2, 3)

# Coded variant of G
female <- rbinom(n = pop_size, size = 1, p = 0.5) 
L1 <- sample(x = score_values, size = pop_size, replace = TRUE)
L2 <- sample(x = score_values, size = pop_size, replace = TRUE) + female
L3 <- sample(x = score_values, size = pop_size, replace = TRUE) + (-1 * female + 1)
X1 <- rbinom(n = pop_size, size = 1, p = (0.2 + 0.6*female))
X2 <- rbinom(n = pop_size, size = 1, p = (0.2 + 0.6*(-1 * female + 1)))

df <- data.frame(female, L1, L2, L3, X1, X2)

summary(df)

df %>%
  filter(female == 0) %>%
  summary()

df %>%
  filter(female == 1) %>%
  summary()
