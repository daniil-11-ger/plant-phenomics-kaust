# Plant Phenomics Analysis - KAUST Internship Project
# Author: Daniil Gerasimov
# Purpose: Statistical analysis of field traits and variety comparison

library(tidyverse)

# 1. Load Data
# Assuming the file 'Plant_Trait_Dataset.csv' is in the root folder
df <- read.csv("Plant_Trait_Dataset.csv", header = TRUE)

# 2. Automated Processing Function (The "Conner" Project logic)
# This function automates mean and SD calculation for specific traits
summarise_trait <- function(df, trait_name, prefix) {
  df %>%
    filter(trait == trait_name) %>%
    mutate(value = as.numeric(value)) %>%
    group_by(plot) %>%
    summarise(
      !!paste0("mean_", prefix) := mean(value, na.rm = TRUE),
      !!paste0("sd_", prefix) := sd(value, na.rm = TRUE),
      .groups = "drop"
    )
}

# 3. Statistical Analysis: Species Comparison
# Comparing Height_cm between two species
species_a <- df %>% filter(Species == "Plantus robusta") %>% select(Height_cm)
species_b <- df %>% filter(Species == "Verdanta minima") %>% select(Height_cm)

t_test_results <- t.test(species_a, species_b)
print(t_test_results)

# 4. Data Visualization
# Histogram of Distribution
ggplot(df, aes(x = Height_cm)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of Plant Height", x = "Height (cm)", y = "Frequency")

# Regression: Leaf Count vs Height
ggplot(df, aes(x = Height_cm, y = Leaf_Count)) +
  geom_point(color = "forestgreen", alpha = 0.6) + 
  geom_smooth(method = "lm", color = "red") + 
  theme_minimal() +
  labs(title = "Correlation: Height vs Leaf Count")
