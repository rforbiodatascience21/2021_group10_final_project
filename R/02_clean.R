# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library(tidyverse)


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
fecal_metabolites <- read_tsv(file = "data/01_fecal_metabolites.tsv")
serum_metabolites <- read_tsv(file = "data/01_serum_metabolites.tsv")
urine_metabolites <- read_tsv(file = "data/01_urine_metabolites.tsv")
GI_behavior <- read_tsv(file = "data/01_GI_behavior.tsv")
immune_microbiota <- read_tsv(file = "data/01_immune_microbiota.tsv")
microbiota_ras <- read_tsv(file = "data/01_microbiota_ras.tsv")

# Wrangle data ------------------------------------------------------------

# Renaming metabolite columns to contain metabolite origin

fecal_metabolites <- fecal_metabolites %>% 
  suffix_numeric_cols(string = "_fecal") %>% 
  mutate(Sample = tolower(Sample))

urine_metabolites <- urine_metabolites %>% 
  suffix_numeric_cols(string = "_urine")

serum_metabolites <- serum_metabolites %>% 
  suffix_numeric_cols(string = "_serum") %>% 
  # Rename column to match column name in other dataframes
  rename("Mixer" = `Probiotic Mixer`) %>% 
  # Change to same nomenclature as in other dataframes
  mutate(Mixer = case_when(Mixer == "yes" ~ "probiotic",
                           Mixer == "no" ~ "non-probiotic"))

      
# Remove and rename relevant columns and rows in microbiota data

microbiota_ras <- microbiota_ras %>% 
  select(-BarcodeSequence) %>% 
  # Change Mixer and Baseline treatment values that are wrong
  mutate(BaseTreat = replace(BaseTreat,
                             Sub_vis == "212_v1",
                             "BL1Syn")) %>% 
  mutate(ProMix = replace(ProMix,
                          Sub_vis == "205_v4",
                          "probiotic")) %>% 
  rename("Sample" = Sub_vis,
         "Baseline Treatment" = BaseTreat,
         "Mixer" = ProMix) %>% 
  # Remove rows that belong to negative controls
  filter(str_detect(Subject,
                    "^neg",
                    negate = TRUE))  


# Pivot long immune_microbiota
immune_microbiota <- immune_microbiota %>% 
  pivot_longer(cols = -c(Subject,
                         Treatment,
                         Arm),    
               names_to = c(".value",
                            "Timing"),
               names_pattern = "(.*)_(.*)")

# Create GI_behavior table without (wo) stool and diff columns, 
# then pivot longer
GI_behavior_wo_stool <- GI_behavior %>% 
  select(-contains("Stool"),
         -contains("_Diff")) %>% 
  pivot_longer(cols = -c(Subject,
                         Treatment,
                         Arm,
                         Order),    
               names_to = c(".value",
                            "Timing"),
               names_pattern = "(.*)_(.*)")

# Make separate table for stool data
stool_data = GI_behavior %>% 
  select("Subject",
         "Treatment",
         "Arm",
         "Order",
         contains("Stool"))


# Write data --------------------------------------------------------------

write_tsv(x = fecal_metabolites,
          file = "data/02_fecal_metabolites_clean.tsv")

write_tsv(x = urine_metabolites,
          file = "data/02_urine_metabolites_clean.tsv")

write_tsv(x = serum_metabolites,
          file = "data/02_serum_metabolites_clean.tsv")

write_tsv(x = microbiota_ras,
          file = "data/02_microbiota_ras_clean.tsv")

write_tsv(x = immune_microbiota,
          file = "data/02_immune_microbiota_clean.tsv")

write_tsv(x = GI_behavior_wo_stool,
          file = "data/02_GI_behavior_wo_stool.tsv")

write_tsv(x = stool_data,
          file = "data/02_stool_data.tsv")
