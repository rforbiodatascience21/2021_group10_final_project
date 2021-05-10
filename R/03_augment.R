# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library(tidyverse)


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
fecal_metabolites = read_tsv(file = "data/02_fecal_metabolites_clean.tsv")
serum_metabolites = read_tsv(file = "data/02_serum_metabolites_clean.tsv")
urine_metabolites = read_tsv(file = "data/02_urine_metabolites_clean.tsv")
GI_behavior = read_tsv(file = "data/02_GI_behavior_wo_stool.tsv")
immune_microbiota = read_tsv(file = "data/02_immune_microbiota_clean.tsv")
microbiota_ras = read_tsv(file = "data/02_microbiota_ras_clean.tsv")


# Wrangle data ------------------------------------------------------------
# Combining all metabolite and microbiota data
metabolites_microbiota = fecal_metabolites %>%
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
                   "Baseline Treatment")) %>% 
  mutate(Subject = replace(Subject,
                           Sample == "208_v3",
                           "208"),
         Visit = replace(Visit,
                         Sample == "208_v3",
                         "3"))

# Divide columns that contain information about >1 variable
# into several columns and remove redundant columns

metabolites_microbiota = metabolites_microbiota %>% 
  separate(col = Treatment,
           into = c("Timing",
                    "Treatment"),
           sep = -3) %>% 
  mutate(Arm = case_when(Visit == 1 | Visit == 2 ~ 1,
                         Visit == 3 | Visit == 4 ~ 2)) %>% 
  select(-"Baseline Treatment",
         -Description,
         -Baseline,
         -"Overall Improvement Q",
         -"Overall Improvement C",
         -"Improved DO Overall",
         -"Improved Pain Overall")


# Merge immune_microbiota, GI_behavior, and metabolites_microbiota data
final_data = metabolites_microbiota %>%
  mutate(Subject = as.double(Subject)) %>% 
  full_join(immune_microbiota,
            by = c("Subject",
                   "Treatment",
                   "Arm",
                   "Timing")) %>% 
  full_join(GI_behavior,
            by = c("Subject",
                   "Treatment",
                   "Arm",
                   "Timing")) %>% 
  mutate(Subject = as.factor(Subject))

# Reorder the columns (with the categorical data at the start)
final_data = final_data %>%
  relocate("#SampleID") %>%
  relocate(Subject, .after = "#SampleID") %>%
  relocate(Sample, .after = Subject) %>%
  relocate(Visit, .after = Sample) %>%
  relocate(Timing, .after = Visit) %>%
  relocate(Treatment, .after = Timing) %>%
  relocate(Mixer, .after = Treatment) %>%
  relocate(Arm, .after = Mixer) %>%
  relocate(Order, .after = Arm) %>%
  relocate(Run, .after = Order)


# Write data --------------------------------------------------------------
write_tsv(x = final_data,
          file = "data/03_final_data_clean_aug.tsv")