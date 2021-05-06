# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library(tidyverse)
library(readxl)


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
download.file(url = "https://doi.org/10.1371/journal.pone.0210064.s006",
              destfile = "data/_raw/raw_sanctuary_data")

# Loading all sheets into separate tables
sanctuary_data_GI_behavior = read_excel("data/_raw/raw_sanctuary_data",
                                         sheet = "GI&Behavior")
sanctuary_data_immune_microbiota = read_excel("data/_raw/raw_sanctuary_data",
                                               sheet = "Immune&Microbiota")
sanctuary_data_fecal_metabolites = read_excel("data/_raw/raw_sanctuary_data",
                                               sheet = "FecalMetabolites")
sanctuary_data_urine_metabolites = read_excel("data/_raw/raw_sanctuary_data",
                                               sheet = "UrineMetabolites")
sanctuary_data_serum_metabolites = read_excel("data/_raw/raw_sanctuary_data",
                                               sheet = "SerumMetabolites")
sanctuary_data_microbiota_ras = read_excel("data/_raw/raw_sanctuary_data",
                                            sheet = "MicrobiotaRAs",
                                            na = "NA")

# Write data --------------------------------------------------------------
write_tsv(x = sanctuary_data_fecal_metabolites,
          file = "data/01_fecal_metabolites.tsv")
write_tsv(x = sanctuary_data_GI_behavior,
          file = "data/01_GI_behavior.tsv")
write_tsv(x = sanctuary_data_immune_microbiota,
          file = "data/01_immune_microbiota.tsv")
write_tsv(x = sanctuary_data_microbiota_ras,
          file = "data/01_microbiota_ras.tsv")
write_tsv(x = sanctuary_data_serum_metabolites,
          file = "data/01_serum_metabolites.tsv")
write_tsv(x = sanctuary_data_urine_metabolites,
          file = "data/01_urine_metabolites.tsv")