# In this script, the population is filtered through the discriminatory filter
library(tidyverse)
set.seed(666)

# Read population
df <- read_rds(file = "data/population.rds")
pop_size <- nrow(df)

# Discrimination step
selection_df <- df %>%
  mutate(rating = pmin(test_score + ifelse(nationality == "Dutch", 0.8, 0), 10),
         accepted = rating >= 8.5)

write_csv(x = selection_df, file = "data/selection.csv")
write_rds(x = selection_df, file = "data/selection.rds")