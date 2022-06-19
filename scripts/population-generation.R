# This script generates the population with variables G L1, L2, L3, X1 and X2.
library(tidyverse)
set.seed(666)

pop_size <- 1000

generate_population <- function(pop_size){
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
}

# Training data
training_data <- generate_population(1000)

# Information about the DF
summary(training_data)
summary(training_data %>% filter(nationality == "Dutch"))
summary(training_data %>% filter(nationality == "Non_Dutch"))

# Save to csv
write_csv(x = training_data, file = "data/population.csv")
write_rds(x = training_data, file = "data/population.rds")

# Test data
test_data <- generate_population(1000)
# Information about the DF
summary(test_data)
summary(test_data %>% filter(nationality == "Dutch"))
summary(test_data %>% filter(nationality == "Non_Dutch"))

# Save to csv
write_csv(x = test_data, file = "data/population_test.csv")
write_rds(x = test_data, file = "data/population_test.rds")
