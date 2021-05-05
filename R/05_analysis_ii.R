# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library(tidyverse)
library(broom)

# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
sanctuary_data <- read_tsv(file = "data/02_clean_data.tsv")

# Wrangle data ------------------------------------------------------------

metabolite_data <- sanctuary_data %>% 
  select(Sample,
         Subject,
         Treatment,
         contains(c("_fecal", "_urine", "_serum"))) %>% 
  filter(Subject != 208)

# Filter out rows 17, 18, 19, and 32!

# Augment data ------------------------------------------------------------

pca_metabolites = metabolite_data %>% 
  select(-Sample,
         -Subject,
         -Treatment) %>% 
  scale() %>% 
  na.omit() %>% 
  prcomp()

# Model data --------------------------------------------------------------


# Visualize data ----------------------------------------------------------

pca_metabolites %>%
  augment(metabolite_data) %>% 
  ggplot(aes(.fittedPC1, .fittedPC2, color = Treatment)) + 
  geom_point(size = 1.5)

# Write data --------------------------------------------------------------
write_tsv()
ggsave()
