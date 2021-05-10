# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library(tidyverse)
library(broom)

# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
sanctuary_data <- read_tsv(file = "data/03_final_data_clean_aug.tsv")

# Wrangle data ------------------------------------------------------------

metabolite_data <- sanctuary_data %>% 
  select(Sample,
         Subject,
         Treatment,
         contains(c("_fecal",
                    "_urine",
                    "_serum"))) %>% 
  filter(Subject != 208) %>% 
  mutate(Subject = as.factor(Subject))


# Model data --------------------------------------------------------------

pca_metabolites <- metabolite_data %>% 
  select(-Sample,
         -Subject,
         -Treatment) %>% 
  scale() %>% 
  prcomp()

# Visualize data ----------------------------------------------------------

pca_metabolites_plot <- pca_metabolites %>%
  augment(metabolite_data) %>% 
  ggplot(aes(.fittedPC1,
             .fittedPC2,
             color = Subject)) + 
  geom_point(size = 2.5,
             aes(shape = Treatment)) +
  stat_ellipse(alpha = 0.5)+
  theme_minimal() +
  labs(x = "PC1",
       y = "PC2",
       title = "PCA of metabolites from fecal, urine, and serum samples")

# Write data --------------------------------------------------------------

ggsave(filename = "05_PCA_metabolites_plot.png",
       path = "results",
       plot = pca_metabolites_plot,
       width = 12,
       height = 8)
