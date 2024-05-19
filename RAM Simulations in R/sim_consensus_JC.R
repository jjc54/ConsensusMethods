library(tidyverse)
library(irr)
library(gtsummary)

# Function to simulate probability for an expert based on their stubbornness and directional bias
expert <- function(seed) {
  set.seed(seed)
  stubbornness <- sample(0:1, size = 1)  # 0: not sure, 1: sure (stubbornness level)
  appro_direction <- sample(0:1, size = 1)  # 0: prefers high scores (8/9), 1: prefers low scores (1/2)
  if (stubbornness == 1) {
    if (appro_direction == 1) {
      prob <- c(0.33, 0.33, 0.34, rep(0, 6))
    } else {
      prob <- c(rep(0, 6), 0.34, 0.33, 0.33)
    }
  } else {
    prob <- c(rep(0, 3), 0.33, 0.34, 0.33, rep(0, 3))
  }
  return(prob)
}

# Function to generate responses and calculate median and kappa statistic
get_median_ram <- function() {
  expert_ids <- sample(1:99999, size = 9)
  probs <- map(expert_ids, expert)  # Apply the expert function to each expert ID
  
  # Generate response frequencies for each expert and convert to actual Likert scale responses
  data_sim <- map_dfr(probs, ~ {
    counts <- rmultinom(n = 20, size = 1, prob = .)
    response <- map_dbl(seq_len(ncol(counts)), ~ which.max(counts[, .x]))
    return(tibble(response))
  }, .id = "expertID")
  
  # Handling pivot wider correctly
  if (is.null(data_sim) || nrow(data_sim) == 0) {
    return(tibble())  # Return an empty tibble if there's an error in response generation
  }
  
  # Compute median for each expert and Fleiss' Kappa for inter-rater reliability
  results <- data_sim %>%
    group_by(expertID) %>%
    summarise(Median = median(response, na.rm = TRUE)) %>%
    ungroup()
  
  # Prepare data for kappa calculation
  t_data <- pivot_wider(data_sim, names_from = expertID, values_from = response, values_fn = list(response = list))
  if (!is.null(t_data) && ncol(t_data) > 1) {
    # Flatten the list to take the first response if there are duplicates
    t_data <- map_df(t_data, ~ .x[[1]])
    results$kappa <- kappam.fleiss(as.matrix(t_data))$value
  } else {
    results$kappa <- NA  # Assign NA to kappa if data is not sufficient for calculation
  }
  
  return(results)
}

# Simulate the process 10 times and convert to a tibble
sim_results <- replicate(1000, get_median_ram(), simplify = FALSE) %>%
  bind_rows() %>%
  as_tibble()

print(sim_results)

str(sim_results)

sim_results %>%
  tbl_summary( #gtSummary Table
    by=expertID,
    type = list(
      c('Median', 'kappa') ~ 'continuous2'),
    statistic = all_continuous2() ~ c(
      "{mean} ± {sd}",
      "{median} ({p25}, {p75})",
      "{min}, {max}"
    ),
    digits = all_continuous2() ~ 3,
    missing="ifany",
  ) %>%
  bold_labels %>%
  italicize_levels()
