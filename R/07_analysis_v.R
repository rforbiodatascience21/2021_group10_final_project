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
         Butyrate_fecal,
         Propionate_fecal,
         Acetate_serum,
         Acetate_urine,
         IL10_CD4,
         TNFa_CD8,
         IL13_CD4)
         
  rename("Fecal butyrate" = Butyrate_fecal,
         "Serum acetate" = Acetate_serum,
         "Urine acetate" = Acetate_urine,
         "Fecal proprionte" = Propionate_fecal)
  

# Visualize data ----------------------------------------------------------
# Scatterplot visualization of correlation between
# certain gut metabolites and immune cells and cytokines




           
    

 

  
  