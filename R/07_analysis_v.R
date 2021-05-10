rm(list = ls())


# Load libraries ----------------------------------------------------------
library(tidyverse)
library(patchwork)

# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
data <- read_tsv(file = "data/02_clean_data.tsv")


# Wrangle data ------------------------------------------------------------
# Subsetting the table for analysis
Gut_metabolites_immune_cells_cytokines_data <- data %>% 
  select(Subject,
         Sample,
         Timing,
         Treatment,
         Acetate_fecal,
         Butyrate_fecal,
         Propionate_fecal,
         Acetate_serum,
         Acetate_urine,
         CD25_M,
         CD25_PMA,
         IL10_CD4,
         IFNg_CD4,
         IFNg_CD8,
         IL17_CD4,
         TNFa_CD4,
         TNFa_CD8,
         IL6_CD4,
         starts_with("ABC"), 
         P_Actinobacteria,
         G_Bifidobacterium,
         contains("g__Akkermansia"),
         G_Clostridium) %>% 
  rename("Fecal acetate" = Acetate_fecal,
         "Fecal butyrate" = Butyrate_fecal,
         "Serum acetate" = Acetate_serum,
         "Urine acetate" = Acetate_urine,
         "Abberant Behavior Score" = ABC_Total,
         "Actinobacteria" = P_Actinobacteria,
         "Bifidobacterium" = G_Bifidobacterium,
         "Clostridium" = G_Clostridium,
         "Akkermansia" = `k__Bacteria;p__Verrucomicrobia;c__Verrucomicrobiae;o__Verrucomicrobiales;f__Verrucomicrobiaceae;g__Akkermansia`) %>% 
  mutate(Treatment = case_when(Treatment == "Pre" ~ "Prebiotic",
                               Treatment == "Syn" ~ "Synbiotic"))


