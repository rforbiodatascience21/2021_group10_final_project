# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library(tidyverse)


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
my_data_clean_aug <- read_tsv(file = "data/03_my_data_clean_aug.tsv")


# Wrangle data ------------------------------------------------------------
my_data_clean_aug %>% ...


# Model data
my_data_clean_aug %>% ...


# Visualize data ----------------------------------------------------------

# Playing with visualizing microbiome compositions in barplot
final_data %>% 
  select(Sample, starts_with("k__")) %>% 
  filter(!str_detect(Sample, "^neg")) %>% 
  pivot_longer(cols = -Sample,
               names_to = "Taxa",
               values_to = "Relative abundance") %>% 
  ggplot(data = .,
         mapping = aes(x = Sample,
                       y = `Relative abundance`,
                       fill = Taxa))+
  geom_col(show.legend = FALSE,
           width = 0.9)


# Write data --------------------------------------------------------------
write_tsv(...)
ggsave(...)