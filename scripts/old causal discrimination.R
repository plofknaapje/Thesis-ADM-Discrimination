# Old causal discrimination script

simple_cd_df <- df %>%
  mutate(test_score = round(test_score/0.5)*0.5) %>%
  group_by(nationality, test_score) %>%
  summarise(accepted_prop = sum(accepted)/n(),
            total = n()) %>%
  pivot_wider(names_from=nationality, values_from=accepted_prop:total) %>%
  mutate(differential = accepted_prop_Dutch - accepted_prop_Non_Dutch,
         size = total_Dutch + total_Non_Dutch) %>%
  select(test_score, differential, size)

print(simple_cd_df)

# Complicated version
accepted_prop <- df %>% # For each group, calculate the proportion accepted
  mutate(test_score = round(test_score)) %>%
  group_by(nationality, test_score, english_cert, extracurricular) %>%
  summarise(accepted_prop = sum(accepted)/n())

cd_df <- df %>% # DF preparation
  mutate(test_score = round(test_score)) %>% # Reduce complexity
  group_by(nationality, test_score, english_cert, extracurricular, accepted) %>%
  summarise(total = n()) %>% # Calculate subgroup sizes
  group_by(test_score, english_cert, extracurricular, accepted) %>%
  mutate(pair_total = sum(total), # Calculate group sizes
         prop = total/pair_total,
         no_comparison = total == pair_total) %>% # Determine groups without counterpart
  ungroup() 

cd_df %>% # Summarise groups with and without counterparts
  group_by(no_comparison) %>%
  summarise(sum(total))

differential_groups <- cd_df %>%
  filter(!no_comparison) %>% # Only use groups with counterpart
  left_join(accepted_prop) %>% # Add acceptance proportion
  select(nationality, test_score, english_cert, extracurricular, pair_total, accepted_prop) %>%
  # Fold Dutch and Non-Dutch acceptance next to eachother
  pivot_wider(names_from = nationality, names_prefix = "accepted_", values_from = accepted_prop) %>%
  mutate(differential = accepted_Dutch - accepted_Non_Dutch) %>% # Difference in treatment between Dutch and Non-Dutch
  filter(differential != 0)