# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library(tidyverse)
library(RColorBrewer)
library(haven)


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
sanctuary_data <- read_tsv(file = "data/02_clean_data.tsv")


# Wrangle data ------------------------------------------------------------

# Subset data to pick out microbiome compositions and make
# data long for plotting

microbiome_data = sanctuary_data %>% 
  select(Sample, starts_with("k__")) %>% 
  pivot_longer(cols = -Sample,
               names_to = "Taxa",
               values_to = "Relative_abundance") %>% 
  mutate(Taxa = str_remove_all(Taxa,
                               "[kpcofg]__"),
         Taxa = str_remove_all(Taxa,
                               "\\["),
         Taxa = str_remove_all(Taxa,
                                "\\]")) %>% 
  separate(col = Taxa,
           into = c("Kingdom", "Phylum", "Class_taxa", "Order_taxa", "Family", "Genus"),
           sep = ";") %>% 
  drop_na() %>% 
  mutate(Phylum = zap_empty(Phylum),
         Class_taxa = zap_empty(Class_taxa),
         Order_taxa = zap_empty(Order_taxa),
         Family = zap_empty(Family),
         Genus = zap_empty(Genus))


# Visualize data ----------------------------------------------------------

# Visualizing microbiome compositions in barplot

ggplot(data = microbiome_data,
       mapping = aes(x = Sample,
                     y = Relative_abundance,
                     fill = Order_taxa))+
  geom_col()+
  scale_fill_manual(values = rep(brewer.pal(11,
                                            "Paired"),
                                 times = 5),
                    aesthetics = "fill")+
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 1))


# Write data --------------------------------------------------------------
write_tsv(...)
ggsave(...)
