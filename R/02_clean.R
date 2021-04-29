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
  rename_with(.cols = where(is.numeric),
              ~ str_c(.,
                      "_fecal"))

urine_metabolites <- urine_metabolites %>% 
  rename_with(.cols = where(is.numeric),
              ~ str_c(.,
                      "_urine"))

serum_metabolites <- serum_metabolites %>% 
  rename_with(.cols = where(is.numeric),
              ~ str_c(.,
                      "_serum")) %>% 
  # Rename column to match column name in other dataframes
  rename(Mixer = "Probiotic Mixer") %>% 
  # Change to same nomenclature as in other dataframes
  mutate(Mixer = case_when(Mixer == "yes" ~ "probiotic",
                           Mixer == "no" ~ "non-probiotic"))

# Remove and rename relevant columns and rows in microbiota data
microbiota_ras <- microbiota_ras %>% 
  select(-BarcodeSequence) %>% 
  # Change Mixer and Baseline treatment values that are wrong
  mutate(BaseTreat=replace(BaseTreat, Sub_vis == "212_v1", "BL1Syn")) %>% 
  mutate(ProMix=replace(ProMix, Sub_vis == "205_v4", "probiotic")) %>% 
  rename(Sample = Sub_vis,
         "Baseline Treatment" = BaseTreat,
         Mixer = ProMix) %>% 
  # Remove rows that belong to negative controls
  filter(!str_detect(Subject,"^neg")) 
  
# Combining all metabolite and microbiota data
metabolites_microbiota <- fecal_metabolites %>%
  mutate(Sample = tolower(Sample)) %>% 
  full_join(microbiota_ras,
            by = c("Sample",
                   "Treatment",
                   "Baseline",
                   "Baseline Treatment",
                   "Mixer")) %>% 
  full_join(serum_metabolites,
            by = c("Sample",
                   "Treatment",
                   "Baseline",
                   "Baseline Treatment",
                   "Mixer")) %>% 
  full_join(urine_metabolites,
            by = c("Sample",
                   "Treatment",
                   "Baseline",
                   "Baseline Treatment"))

# Divide columns that contain information about >1 variable
# into several columns and remove redundant columns

metabolites_microbiota <- metabolites_microbiota %>% 
  separate(col = Treatment,
           into = c("Timing", "Treatment"),
           sep = -3) %>% 
  mutate(Arm = case_when(Visit == 1 | Visit == 2 ~ 1,
                         Visit == 3 | Visit == 4 ~ 2)) %>% 
  select(-"Baseline Treatment",
         -Description,
         -Baseline)

# Write data --------------------------------------------------------------
write_tsv(x = metabolites_microbiota,
          file = "data/02_metabolites_microbiota.tsv")
