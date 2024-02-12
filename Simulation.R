# Install packages if not already installed
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("psych")) install.packages("psych")
if (!require("irr")) install.packages("irr")

# Load the packages
library(tidyverse)
library(psych)
library(irr)

# Define simulation parameters
num_items <- 10  # Number of clinical scenarios to be rated
rating_scale <- 1:9  # Rating scale (1-9 for RAM)
panel_sizes <- c(5, 9, 15)  # Different panel sizes to simulate
num_simulations <- 100  # Number of simulations per configuration

simulate_panel_ratings <- function(panel_size, num_items, rating_scale) {
  ratings <- matrix(sample(rating_scale, panel_size * num_items, replace = TRUE), 
                    nrow = panel_size, ncol = num_items)
  return(ratings)
}

set.seed(123)  # For reproducibility

# Initialize a data frame to store results
simulation_results <- data.frame()

set.seed(123)  # For reproducibility

# Initialize a data frame to store results
simulation_results <- data.frame()

# Loop over panel sizes
for (size in panel_sizes) {
  for (i in 1:num_simulations) {
    # Simulate panel ratings
    ratings <- simulate_panel_ratings(size, num_items, rating_scale)
    
    # Try-Catch block for error handling
    tryCatch({
      # Alternative ICC calculation using the 'irr' package
      irr_result <- icc(t(ratings))
      if (is.null(irr_result$value)) {
        irr <- NA  # Assign NA if ICC calculation fails
      } else {
        irr <- irr_result$value  # Use the ICC value
      }
    }, warning = function(w) {
      irr <- NA  # Assign NA in case of warning
    }, error = function(e) {
      irr <- NA  # Assign NA in case of error
    })
    
    # Store the results
    simulation_results <- rbind(simulation_results, data.frame(PanelSize = size, IRR = ifelse(is.na(irr), NA, irr)))
  }
}

# Summary statistics
simulation_summary <- simulation_results %>%
  group_by(PanelSize) %>%
  summarise(MeanIRR = mean(IRR), SDIRR = sd(IRR))

print(simulation_summary)

# Visualization
ggplot(simulation_results, aes(x = factor(PanelSize), y = IRR)) +
  geom_boxplot() +
  labs(title = "Impact of Panel Size on IRR", x = "Panel Size", y = "Intraclass Correlation Coefficient (IRR)") +
  theme_minimal()
