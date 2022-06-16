# This script generates the population with variables G L1, L2, L3, X1 and X2.
library(tidyverse)
set.seed(666)

pop_size <- 1000

# Coded variant of G
nationality <- sample(as.factor(c('Dutch', 'Non_Dutch')), pop_size, replace = TRUE)
gender <- sample(as.factor(c('Male', 'Female')), pop_size, replace = TRUE)
test_score <- rnorm(n = pop_size, mean = 6.5, sd = 1.5) %>%
  pmax(1) %>%
  pmin(10) %>%
  round(digits=1)
english_cert_dutch = sample(c(T, F), pop_size, replace = TRUE, prob = c(0.1, 0.9))
english_cert_int = sample(c(T, F), pop_size, replace = TRUE, prob = c(0.6, 0.4))
extracurricular_dutch = sample(c(T, F), pop_size, replace = TRUE, prob = c(0.2, 0.8))
extracurricular_int = sample(c(T, F), pop_size, replace = TRUE, prob = c(0.6, 0.4))

# turn vectors into DF
df <- data.frame(nationality, gender, test_score) %>%
  mutate(english_cert = ifelse(nationality == "Dutch", 
                               english_cert_dutch, english_cert_int),
         extracurricular = ifelse(nationality == "Dutch", 
                                  extracurricular_dutch, extracurricular_int))

# Information about the DF
summary(df)
summary(df %>% filter(nationality == "Dutch"))
summary(df %>% filter(nationality == "Non-Dutch"))

# Save to csv
write_csv(x = df, file = "data/population.csv")
write_rds(x = df, file = "data/population.rds")
