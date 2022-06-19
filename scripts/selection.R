# In this script, the population is filtered through the discriminatory filter
library(tidyverse)
set.seed(666)

discriminate <- function(df){
  df %>% mutate(rating = pmin(test_score + 
                                ifelse(nationality == "Dutch", 0.8, 0), 10),
                accepted = rating >= 8.5)
}
# Read population
df <- read_rds(file = "data/population.rds")
df_test <- read_rds(file = "data/population_test.rds")

# Discrimination step
df <- discriminate(df)
df_test <- discriminate(df_test)

write_csv(x = df, file = "data/selection.csv")
write_rds(x = df, file = "data/selection.rds")

write_csv(x = df_test, file = "data/selection_test.csv")
write_rds(x = df_test, file = "data/selection_test.rds")
