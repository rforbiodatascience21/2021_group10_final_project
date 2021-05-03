# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library(tidyverse)
library(cowplot)
library(ggpubr)
library(RColorBrewer)


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
my_data_clean_aug <- read_tsv(file = "data/03_my_data_clean_aug.tsv")


# Wrangle data ------------------------------------------------------------
my_data_clean_aug %>% ...


# Model data
my_data_clean_aug %>% ...


# Visualize data ----------------------------------------------------------

# Creating more colors for the microbiome plot
mycolors <- colorRampPalette(brewer.pal(9, "Paired"))(34)

# Playing with visualizing microbiome compositions in barplot
final_data %>% 
  select(Sample, starts_with("k__")) %>% 
  filter(!str_detect(Sample, "^neg")) %>% 
  pivot_longer(cols = -Sample,
               names_to = "Taxa",
               values_to = "Relative_abundance") %>% 
  separate(col = Taxa,
           into = c("Kingdom", "Phylum", "Class_taxa", "Order_taxa", "Family", "Genus"),
           sep = ";") %>% 
  drop_na() %>% 
  ggplot(data = .,
         mapping = aes(x = Sample,
                       y = Relative_abundance,
                       fill = Class_taxa))+
  geom_col()+
  scale_fill_manual(values = mycolors,
                    aesthetics = "fill")
                     


# Write data --------------------------------------------------------------
write_tsv(...)
ggsave(...)
