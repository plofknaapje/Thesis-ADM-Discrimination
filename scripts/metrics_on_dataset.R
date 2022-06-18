# In this script, I calculate the fairness metrics on the selection dataset.
library(tidyverse)
library(tidymodels)
set.seed(666)

# == Functions =============================================
group_fairness <- function(df, sensitive, outcome, fitted_model=NULL) {
  # Sum each outcome for each value of sensitive and calculate the relative probabilities.
  if (is.null(fitted_model)) {
    summary <- df %>%
      group_by({{sensitive}}, {{outcome}}) %>%
      summarise(total = n()) %>%
      group_by({{sensitive}}) %>%
      mutate(proportion = total/sum(total)) %>%
      ungroup()
    list(summary, df)
  } else {
    summary <- df %>%
      mutate(outcome = predict(fitted_model, df)[[".pred_class"]]) %>%
      group_by({{sensitive}}, {{outcome}}) %>%
      summarise(total = n()) %>%
      group_by({{sensitive}}) %>%
      mutate(proportion = total/sum(total)) %>%
      ungroup()
    list(summary, df)
  }
  
}

causal_discrimination <- function(df, fitted_model){
  # Determine the number of applicants who get a different outcome depending on 
  # their nationality in the fitted model.
  
  pop_size <- nrow(df)
  
  # Flip nationalities
  inverted_df <- df %>% 
    mutate(nationality = ifelse(nationality == "Dutch", "Non_Dutch", "Dutch"))
  
  # Add prediction column
  eval_df <- df %>% 
    mutate(predicted = predict(fitted_model, df)[[".pred_class"]],
           inverted_predicted = predict(fitted_model, inverted_df)[[".pred_class"]],
           different = predicted != inverted_predicted)
  
  list(sum(eval_df$different)/pop_size, eval_df)
}

all_metrics <- function(df, model, original_df=NULL) {
  pop_size <- nrow(df)
  results <- vector(mode="list", length = 3)
  
  # Group fairness
  gf_df <- df %>%
    mutate(accepted = as.factor(accepted)) 
  
  gf_model <- model %>%
    fit(accepted ~ nationality + test_score + english_cert + extracurricular, data = gf_df)
  
  results[[1]] <- group_fairness(df, nationality, accepted, gf_model)
  
  # Causal discrimination
  caudisc_df <- df %>%
    mutate(accepted = as.factor(accepted))

  caudisc_model <- model %>%
    fit(accepted ~ nationality + test_score + english_cert + extracurricular, data = caudisc_df)
  
  if (!is.null(original_df)) {
    caudisc_df_original <- original_df %>%
      mutate(accepted = as.factor(accepted))

    results[[2]] <- causal_discrimination(caudisc_df_original, caudisc_model)
  } else {
    results[[2]] <- causal_discrimination(caudisc_df, caudisc_model)
  }
  
  # Fairness through unawareness
  unaware_df <- df %>%
    mutate(accepted = as.factor(accepted))
  
  unaware_model <- model %>%
    fit(accepted ~ test_score + english_cert + extracurricular, data = unaware_df)
  
  if (!is.null(original_df)) {
    processed_original_df <- original_df %>%
      mutate(accepted = as.factor(accepted))
    
    eval_df <- original_df %>%
      mutate(predicted_accepted = predict(unaware_model, processed_original_df)[[".pred_class"]],
             different = accepted != predicted_accepted)
  } else {
    eval_df <- unaware_df %>%
      mutate(predicted_accepted = predict(unaware_model, unaware_df)[[".pred_class"]],
             different = accepted != predicted_accepted)
  }

  
  total_accepted = sum(eval_df$predicted_accepted == "TRUE")
  ### Affected group (test_score in [7.7, 8.4])
  different <- filter(eval_df, different)
  
  results[[3]] <- group_fairness(eval_df, nationality, predicted_accepted)
  names(results) <- c("group_fairness", "causal_discrimination", "unawareness")
  results
}

print_all_metrics <- function(title, results) {
  print(title)
  print("Group fairness")
  print(results$group_fairness[[1]])
  print("Causal discrimination")
  print(results$causal_discrimination[[1]])
  print("Unawareness")
  print(results$unawareness[[1]])
}
