# In this script, the population is filtered through the discriminatory filter
library(tidyverse)
set.seed(666)

# Read population
df <- read.csv(file = "data/population.csv")
pop_size <- nrow(df)

# Discrimination step
selection_df <- df %>%
  mutate(bias = (female * -1 + 1) * 0.5,
#         noise = rnorm(n = pop_size, mean = 0, sd = 0.25),
         score = L1 + L2 + L3 + bias) %>%
  arrange(-score) %>%
  mutate(label = c(rep(1, pop_size*0.1), rep(0, pop_size*0.9))) %>%
  select(-bias, -score)

# Information about the DF
summary(selection_df)
summary(filter(selection_df, label == 1))
summary(filter(selection_df, label == 0))

write.csv(x = selection_df, file = "data/selection.csv")