# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
my_data <- read_tsv(file = "data/01_my_data.tsv")


# Wrangle data ------------------------------------------------------------
# Assigning metabolite origin and creating full joined metabolite table
sanctuary_data_fecal_metabolites <- sanctuary_data_fecal_metabolites %>% 
  rename_at(vars(-Treatment, -Sample, -Baseline, -`Baseline Treatment`, -Mixer),
            ~ paste0(.,"_fecal")) 

sanctuary_data_urine_metabolites <- sanctuary_data_urine_metabolites %>% 
  rename_at(vars(-(1:4)),
            ~ paste0(.,"_urine"))

sanctuary_data_serum_metabolites <- sanctuary_data_serum_metabolites %>% 
  rename_at(vars(-(1:22)),
            ~ paste0(.,"_serum"))

sanctuary_data_metabolites <- sanctuary_data_fecal_metabolites %>%
  mutate(Sample = tolower(Sample)) %>% 
  full_join(sanctuary_data_urine_metabolites,
            by = c("Sample", "Treatment", "Baseline", "Baseline Treatment")) %>% 
  full_join(sanctuary_data_serum_metabolites,
            by = c("Sample", "Treatment", "Baseline", "Baseline Treatment"))



# Write data --------------------------------------------------------------
write_tsv(x = my_data_clean,
          file = "data/02_my_data_clean.tsv")