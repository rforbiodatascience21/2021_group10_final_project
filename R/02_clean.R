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
  rename("Mixer" = `Probiotic Mixer`) %>% 
  
  # Change to same nomenclature as in other dataframes
  mutate(Mixer = case_when(Mixer == "yes" ~ "probiotic",
                           Mixer == "no" ~ "non-probiotic"))
      
# Remove and rename relevant columns and rows in microbiota data
microbiota_ras <- microbiota_ras %>% 
  select(-BarcodeSequence) %>% 
  # Change Mixer and Baseline treatment values that are wrong
  mutate(BaseTreat=replace(BaseTreat, Sub_vis == "212_v1", "BL1Syn")) %>% 
  mutate(ProMix=replace(ProMix, Sub_vis == "205_v4", "probiotic")) %>% 
  rename("Sample" = Sub_vis,
         "Baseline Treatment" = BaseTreat,
         "Mixer" = ProMix)
  # Remove rows that belong to negative controls
  filter(!str_detect(Subject,"^neg")) # Should these be removed?
  
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
         -Baseline,
         -"Overall Improvement Q", # removing empty columns
         -"Overall Improvement C",
         -"Improved DO Overall",
         -"Improved Pain Overall")  


# Merge immune and behavior data
GI_behavior_immune <- immune_microbiota %>% 
  full_join(GI_behavior,
            by = c("Subject",
                   "Treatment",
                   "Arm"))

# Merge Behavior_Immune and Metabolites_microbiodata data
final_data = GI_behavior_immune %>%
  mutate(Subject = tolower(Subject)) %>%
  full_join(metabolites_microbiota,
            by = c("Subject",
                   "Treatment",
                   "Arm"))

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

# Pivot Long "Stool" to "Result"
test2 = final_data %>%
  pivot_longer(c(`Stool_Freq_Pre`, `Stool_Norm_Pre`, `Stool_Hard_Pre`, `Stool_Soft_Pre`, `Stool_Freq_D123`, `Stool_Freq_W1`,
                 `Stool_Freq_W5`,`Stool_Freq_D835`,`Stool_Norm_D123`,`Stool_Norm_W1`,`Stool_Norm_W5`,`Stool_Norm_D835`,
                 `Stool_Hard_D123`,`Stool_Hard_W1`,`Stool_Hard_W5`,`Stool_Hard_D835`,`Stool_Soft_D123`,`Stool_Soft_W1`,
                 `Stool_Soft_W5`,`Stool_Soft_D835`), 
               names_to = "Stool", 
               values_to = "Result")


# Pivot long immune_microbiota
immune_microbiota <- immune_microbiota %>% 
  pivot_longer(cols = -c(Subject, Treatment, Arm),    
               names_to = c(".value", "Timing"),
               names_pattern = "(.*)_(.*)")

# Create GI_behavior table without (wo) stool and diff columns, then pivot longer
GI_behavior_wo_stool <- GI_behavior %>% 
  select(-contains("Stool"),
         -contains("_Diff")) %>% 
  pivot_longer(cols = -c(Subject, Treatment, Arm,Order,),    
               names_to = c(".value", "Timing"),
               names_pattern = "(.*)_(.*)")

stool_data <- GI_behavior %>% 
  select("Subject", "Treatment", "Arm", "Order", contains("Stool"))

# Remove stool data from behavior data
behavior_w_stool <- GI_behavior %>% 
  select(-contains("Stool"))

# Make separate table for stool data
stool_data <- GI_behavior %>% 
  select("Subject", "Treatment", "Arm", "Order", contains("Stool"))

# Write data --------------------------------------------------------------
write_tsv(x = metabolites_microbiota,
          file = "data/02_metabolites_microbiota.tsv")

write_tsv(x = GI_behavior_immune,
          file = "data/02_GI_behavior_immune.tsv")
